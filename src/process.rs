use crate::builtins::{InternalCommandContext, InternalCommandError, INTERNAL_COMMANDS};
use crate::eval::*;
use crate::expand::*;
use crate::parser;
use crate::shell::Shell;
use crate::utils::{FdFile, Result, Error};
use crate::variable::Value;
use nix;
use nix::sys::signal::{kill, sigaction, SaFlags, SigAction, SigHandler, SigSet, Signal};
use nix::sys::termios::{tcgetattr, tcsetattr, SetArg::TCSADRAIN, Termios};
use nix::sys::wait::{waitpid, WaitPidFlag, WaitStatus};
use nix::unistd::{close, dup2, execv, fork, getpid, setpgid, tcsetpgrp, ForkResult, Pid};
use std::ffi::CString;
use std::fmt;
use std::fs::{OpenOptions, File, metadata};
use std::hash::{Hash, Hasher};
use std::os::unix::io::IntoRawFd;
use std::os::unix::io::RawFd;
use std::sync::{Arc, Mutex};
use std::io::{Read, SeekFrom, Seek};

// type Result<I> = std::result::Result<I, Error>;

fn kill_process_group(pgid: Pid, signal: Signal) -> Result<()> {
    kill(Pid::from_raw(-pgid.as_raw()), signal).map_err(|e| e.into())
}

fn move_fd(src: RawFd, dst: RawFd) {
    if src != dst {
        dup2(src, dst).expect("failed to dup2");
        close(src).expect("failed to close");
    }
}

pub fn wait_child(pid: Pid) -> Result<i32> {
    let wait_status = waitpid(pid, None)?;
    match wait_status {
        WaitStatus::Exited(_, status) => Ok(status),
        // TODO: Handle errors.
        _ => {
            let msg = format!("waitpid returned an unexpected value: {:?}", wait_status);

            warn!("waitpid: {}", msg);
            Err(Error::Message(msg).into())
        }
    }
}

pub const EXIT_STATUS_CTRL_C: ExitStatus = ExitStatus::ExitedWith(130); // Defined it by bash.

/// The exit status or reason why the command exited.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum ExitStatus {
    ExitedWith(i32),
    Running(Pid /* pgid */),
    Break,
    Continue,
    Return,
    // The command is not executed because of `noexec`.
    NoExec,
    Expected,
}

/// The process execution environment.
#[derive(Debug, Copy, Clone)]
pub struct Context {
    pub stdin: RawFd,
    pub stdout: RawFd,
    pub stderr: RawFd,
    pub pgid: Option<Pid>,
    /// The process should be executed in background.
    pub background: bool,
    /// Is the shell interactive?
    pub interactive: bool,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum ProcessState {
    Running,
    /// Contains the exit status.
    Completed(i32),
    /// Suspended (Ctrl-Z).
    Stopped(Pid),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct JobId(usize);

impl JobId {
    pub fn new(id: usize) -> JobId {
        JobId(id)
    }
}

impl fmt::Display for JobId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.0, f)
    }
}

/// Represents a job.
/// See https://www.gnu.org/software/libc/manual/html_node/Implementing-a-Shell.html
pub struct Job {
    pub id: JobId,
    pub pgid: Pid,
    pub cmd: String,
    // TODO: Remove entries in shell.states on destruction.
    pub processes: Vec<Pid>,
    pub termios: Mutex<Option<Termios>>,
}

impl Job {
    pub fn new(id: JobId, pgid: Pid, cmd: String, processes: Vec<Pid>) -> Job {
        Job {
            id,
            pgid,
            cmd,
            processes,
            termios: Mutex::new(None),
        }
    }

    #[inline]
    pub fn id(&self) -> JobId {
        self.id
    }

    #[inline]
    pub fn cmd(&self) -> &str {
        self.cmd.as_str()
    }

    pub fn state(&self, shell: &Shell) -> &'static str {
        if self.completed(shell) {
            "done"
        } else if self.stopped(shell) {
            "stopped"
        } else {
            "running"
        }
    }

    pub fn completed(&self, shell: &Shell) -> bool {
        self.processes.iter().all(|pid| {
            let state = shell.get_process_state(*pid).unwrap();
            match state {
                ProcessState::Completed(_) => true,
                _ => false,
            }
        })
    }

    pub fn stopped(&self, shell: &Shell) -> bool {
        self.processes.iter().all(|pid| {
            let state = shell.get_process_state(*pid).unwrap();
            match state {
                ProcessState::Stopped(_) => true,
                _ => false,
            }
        })
    }
}

impl PartialEq for Job {
    fn eq(&self, other: &Job) -> bool {
        self.id == other.id
    }
}

impl Eq for Job {}

impl Hash for Job {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

pub fn continue_job(shell: &mut Shell, job: &Arc<Job>, background: bool) {
    // Mark all stopped processes as running.
    for proc in &job.processes {
        if let ProcessState::Stopped(_) = shell.get_process_state(*proc).unwrap() {
            shell.set_process_state(*proc, ProcessState::Running);
        }
    }

    if background {
        run_in_background(shell, &job, true);
    } else {
        run_in_foreground(shell, &job, true);
    }
}

/// Put the given pgid (job) into the foreground.
pub fn set_terminal_process_group(pgid: Pid) {
    tcsetpgrp(0, pgid).expect("failed to tcsetpgrp");
}

fn restore_terminal_attrs(termios: &Termios) {
    tcsetattr(std::io::stdin(), TCSADRAIN, termios).expect("failed to tcsetattr");
}

pub fn run_in_foreground(shell: &mut Shell, job: &Arc<Job>, sigcont: bool) -> ProcessState {
    shell.background_jobs_mut().remove(job);
    set_terminal_process_group(job.pgid);

    if sigcont {
        // if let Some(termios) = termios
        if let Some(termios) = &*job.termios.lock().unwrap() {
            restore_terminal_attrs(termios);
        }
        kill_process_group(job.pgid, Signal::SIGCONT).expect("failed to kill(SIGCONT)");
        trace!("sent sigcont");
    }

    // Wait for the job to exit or stop.
    let status = wait_for_job(shell, &job);

    // Save the current terminal status.
    job.termios.lock().unwrap()
        .replace(tcgetattr(std::io::stdin()).expect("failed to tcgetattr"));

    // Go back into the shell.
    set_terminal_process_group(shell.shell_pgid);
    restore_terminal_attrs(shell.shell_termios.as_ref().unwrap());

    status
}

pub fn run_in_background(shell: &mut Shell, job: &Arc<Job>, sigcont: bool) {
    shell.set_last_back_job(job.clone());
    shell.background_jobs_mut().insert(job.clone());

    if sigcont {
        kill_process_group(job.pgid, Signal::SIGCONT).expect("failed to kill(SIGCONT)");
    }
    writeln!(shell, "[{}] {} {}", job.id, job.pgid, job.cmd).unwrap();
}

pub fn destroy_job(shell: &mut Shell, job: &Arc<Job>) {
    // TODO: Remove processes from shell.pid_job_mapping
    // TODO: I suppose this function should be Drop::drop().

    if shell.background_jobs_mut().remove(job) {
        // The job was a background job. Notify the user that the job
        // has finished.
        // println!("[{}] Done: {}", job.id, job.cmd);
        writeln!(shell, "[{}] {} Done: {}", job.id, job.pgid, job.cmd).unwrap();
    }

    shell.jobs_mut().remove(&job.id).unwrap();

    if let Some(ref last_job) = shell.last_stopped_job {
        if job.id == last_job.id {
            shell.last_stopped_job = None;
        }
    }
}

/// Checks if background jobs have been terminated and notify the user that some jobs
/// have been finished.
pub fn check_background_jobs(shell: &mut Shell) {
    while let Some(pid) = wait_for_any_process(shell, true) {
        let job = shell.get_job_by_pid(pid).unwrap().clone();
        if job.completed(shell) {
            destroy_job(shell, &job);
        } else if job.stopped(shell) {
            // println!("[{}] Done: {}", job.id, job.cmd);
            writeln!(shell, "[{}] {} Done: {}", job.id, job.pgid, job.cmd).unwrap();
        }
    }
}

/// Waits for all processes in the job to exit. Note that the job will be
/// deleted from `shell` if the process has exited.
pub fn wait_for_job(shell: &mut Shell, job: &Arc<Job>) -> ProcessState {
    loop {
        if job.completed(shell) || job.stopped(shell) {
            break;
        }

        wait_for_any_process(shell, false);
    }

    // Get the exit status of the last process.
    let state = shell
        .get_process_state(*job.processes.iter().last().unwrap())
        .cloned();

    match state {
        Some(ProcessState::Completed(_)) => {
            // Remove the job and processes from the list.
            destroy_job(shell, job);
            state.unwrap()
        }
        Some(ProcessState::Stopped(_)) => {
            shell.last_stopped_job = Some(job.clone());
            print_err!("[{}] Stopped: {}", job.id, job.cmd);
            state.unwrap()
        }
        _ => unreachable!(),
    }
}

/// Waits for an *any* process, i.e. `waitpid(-1)`, and then updates
/// the process state recorded in the `shell`. Returns `None` it
/// would block.
pub fn wait_for_any_process(shell: &mut Shell, no_block: bool) -> Option<Pid> {
    let options = if no_block {
        WaitPidFlag::WUNTRACED | WaitPidFlag::WNOHANG
    } else {
        WaitPidFlag::WUNTRACED
    };

    let result = waitpid(None, Some(options));
    let (pid, state) = match result {
        Ok(WaitStatus::Exited(pid, status)) => {
            trace!("exited: pid={} status={}", pid, status);
            (pid, ProcessState::Completed(status))
        }
        Ok(WaitStatus::Signaled(pid, _signal, _)) => {
            // The `pid` process has been killed by `_signal`.
            (pid, ProcessState::Completed(-1))
        }
        Ok(WaitStatus::Stopped(pid, _signal)) => (pid, ProcessState::Stopped(pid)),
        Err(nix::errno::Errno::ECHILD) | Ok(WaitStatus::StillAlive) => {
            // No childs to be reported.
            return None;
        }
        status => {
            panic!("unexpected waitpid event: {:?}", status);
        }
    };

    shell.set_process_state(pid, state);
    Some(pid)
}

fn open_option_new(direction: &parser::RedirectionDirection) -> OpenOptions {
    let mut options = OpenOptions::new();
    match direction {
        parser::RedirectionDirection::Input => {
            options.read(true);
        }
        parser::RedirectionDirection::Output => {
            options.write(true).truncate(true).create(true);
        }
        parser::RedirectionDirection::Append => {
            options.write(true).append(true);
        }
    };
    return options
}

/// Spawn a child process and execute a command.
pub fn run_external_command(
    shell: &mut Shell,
    ctx: &Context,
    argv: Vec<String>,
    redirects: &[parser::Redirection],
    assignments: &[parser::Assignment],
) -> Result<ExitStatus> {
    // Determine the absolute path of the command.
    let argv0 = if argv[0].starts_with('/') || argv[0].starts_with("./") || argv[0].starts_with("../") {
        argv[0].to_owned()
    } else {
        match shell.commands().get_external(&argv[0]) {
            Some(entry) => {
                if let Some(path) = entry.path.to_str() {
                    path.to_owned()
                } else {
                    print_err!("Invalid UTF8 character `{}'", argv[0]);
                    return Ok(ExitStatus::ExitedWith(1));
                }
            }
            None => {
                argv[0].to_owned()
            }
        }
    };

    match metadata(&argv0) {
        Ok(meta) => {
            if meta.is_dir() {
                return run_internal_command(shell, &vec!["cd".into(), argv0], ctx.stdin, ctx.stdout, ctx.stderr, redirects);
            }
        }
        Err(_) => {
            print_err!("command not found `{}`", argv0);
            return Ok(ExitStatus::ExitedWith(255));
        }
    }

    // Construct CString argv.
    let mut args = Vec::new();
    for arg in argv {
        args.push(CString::new(arg)?);
    }

    let mut fds = Vec::new();
    let fds_close = |fds: Vec<(RawFd, RawFd)>| {
        for fd in fds {
            if fd.0 != ctx.stdin && fd.0 != ctx.stdout && fd.0 != ctx.stderr {
                close(fd.0).expect("failed to close");
            }
        }
    };

    for r in redirects {
        match r.target {
            parser::RedirectionType::FileOrFd(ref target) => {
                let target = expand_word_into_string(shell, target)?;
                if let Ok(fd) = target.parse::<i32>() {
                    fds.push((fd, r.fd as RawFd));
                } else {
                    let options = open_option_new(&r.direction);
                    if let Ok(file) = options.open(&target) {
                        fds.push((file.into_raw_fd(), r.fd as RawFd))
                    } else {
                        fds_close(fds);
                        print_err!("failed to open file: `{}'", target);
                        return Ok(ExitStatus::ExitedWith(1));
                    }
                }
            }
            parser::RedirectionType::Fd(ref fd) => {
                fds.push((*fd, r.fd as RawFd));
            }
            parser::RedirectionType::File(ref wfilepath) => {
                let options = open_option_new(&r.direction);

                trace!("redirection: options={:?}", options);
                let filepath = expand_word_into_string(shell, wfilepath)?;
                if let Ok(file) = options.open(&filepath) {
                    fds.push((file.into_raw_fd(), r.fd as RawFd))
                } else {
                    fds_close(fds);
                    print_err!("failed to open file: `{}'", filepath);
                    return Ok(ExitStatus::ExitedWith(1));
                }
            }
            parser::RedirectionType::HereDoc(ref heredoc) => {
                fds.push((evaluate_heredoc(shell, heredoc)?, r.fd as RawFd))
            }
            parser::RedirectionType::UnresolvedHereDoc { .. } => {
                // must be resolved in the parser
                unreachable!()
            }
        }
    }

    // Use provided (e.g. pipeline) stdin/stdout/stderr if no redirections speicfied.
    if !fds.iter().any(|(_, dst)| *dst == 0) {
        fds.push((ctx.stdin, 0));
    }
    if !fds.iter().any(|(_, dst)| *dst == 1) {
        fds.push((ctx.stdout, 1));
    }
    if !fds.iter().any(|(_, dst)| *dst == 2) {
        fds.push((ctx.stderr, 2));
    }

    // Spawn a child.
    match unsafe {fork()}.expect("failed to fork") {
        ForkResult::Parent { child } => {
            fds_close(fds);
            Ok(ExitStatus::Running(child))
        },
        ForkResult::Child => {
            // Create or join a process group.
            if ctx.interactive {
                let pid = getpid();
                let pgid = match ctx.pgid {
                    Some(pgid) => {
                        setpgid(pid, pgid).expect("failed to setpgid");
                        pgid
                    }
                    None => {
                        setpgid(pid, pid).expect("failed to setpgid");
                        pid
                    }
                };

                if !ctx.background {
                    set_terminal_process_group(pgid);
                    restore_terminal_attrs(shell.shell_termios.as_ref().unwrap());
                }

                // Accept job-control-related signals (refer https://www.gnu.org/software/libc/manual/html_node/Launching-Jobs.html)
                let action = SigAction::new(SigHandler::SigDfl, SaFlags::empty(), SigSet::empty());
                unsafe {
                    sigaction(Signal::SIGINT, &action).expect("failed to sigaction");
                    sigaction(Signal::SIGQUIT, &action).expect("failed to sigaction");
                    sigaction(Signal::SIGTSTP, &action).expect("failed to sigaction");
                    sigaction(Signal::SIGTTIN, &action).expect("failed to sigaction");
                    sigaction(Signal::SIGTTOU, &action).expect("failed to sigaction");
                    sigaction(Signal::SIGCHLD, &action).expect("failed to sigaction");
                }
            }

            // Initialize stdin/stdout/stderr and redirections.
            for (src, dst) in fds {
                move_fd(src, dst);
            }

            // Set exported variables.
            for name in shell.exported_names() {
                if let Some(var) = shell.get(name) {
                    std::env::set_var(name, var.as_str());
                }
            }

            // Load assignments.
            for assignment in assignments {
                let value = evaluate_initializer(shell, &assignment.initializer)
                    .expect("failed to evaluate the initializer");
                match value {
                    Value::String(s) => std::env::set_var(&assignment.name, s),
                    Value::Array(_) => {
                        print_err!("Array assignments in a command is not supported.");
                        std::process::exit(1);
                    }
                    Value::Function(_) => (),
                }
            }

            shell_execv(shell, ctx, CString::new(argv0)?, args);
            unreachable!();
        }
    }
}

fn check_binary_file(sample: &[u8]) -> bool {
    for ch in sample {
        if *ch == 0 { return true; }
        if *ch == '\n' as u8 { return false; }
    }
    return false;
}

fn shell_execv(shell: &mut Shell,
    ctx: &Context,
    command: CString,
    args: Vec<CString>,
) {

    let exec_args: Vec<&std::ffi::CStr> = args.iter().map(|s| s.as_c_str()).collect();
    match execv(&command, &exec_args) {
        Ok(_) => {
            unreachable!();
        }
        Err(nix::errno::Errno::EACCES) => {
            print_err!("Failed to exec {:?} (EACCESS). chmod(1) may help.", command);
            std::process::exit(1);
        }
        Err(nix::errno::Errno::ENOEXEC) => {
            let mut f = File::open(&command.to_str().unwrap()).unwrap();
            let mut sample = [0; 80];

            let sample_len = f.read(&mut sample).unwrap();

            if sample_len > 2 && sample[0] == '#' as u8 && sample[1] == '!' as u8 {
                let line_end = sample[2..].iter().position(|v| *v == '\n' as u8).unwrap_or(sample_len);
                let line = if let Ok(line) = std::str::from_utf8(&sample[2..line_end]) {
                    line
                } else {
                    print_err!("Invalid UTF8 character in the first line of `{:?}'", command);
                    std::process::exit(1);
                };

                let mut new_args = Vec::new();
                line.split_ascii_whitespace().for_each(|arg| new_args.push(CString::new(arg).unwrap()));
                new_args.extend(args);
                shell_execv(shell, ctx, new_args[0].to_owned(), new_args);
            } else {
                if check_binary_file(&sample[..sample_len]) == true {
                    print_err!("{:?}: cannot execute binary file", command);
                    std::process::exit(126);
                }
                f.seek(SeekFrom::Start(0)).unwrap();
                let mut script = String::new();
                f.read_to_string(&mut script).unwrap();
                shell.set_interactive(false);
                if let ExitStatus::ExitedWith(status) = shell.run_str(script.as_str(), false) {
                    std::process::exit(status);
                } else {
                    std::process::exit(1);
                }
            };
        }
        Err(err) => {
            let exit_code = if err == nix::errno::Errno::ENOENT {
                127
            } else {
                126
            };
            print_err!("Failed to exec {:?} ({})", command, err);
            std::process::exit(exit_code);
        }
    }
}

/// Runs an internal (builtin) command.
pub fn run_internal_command(
    shell: &mut Shell,
    argv: &[String],
    mut stdin: RawFd,
    mut stdout: RawFd,
    mut stderr: RawFd,
    redirects: &[parser::Redirection],
) -> Result<ExitStatus> {
    let func = match INTERNAL_COMMANDS.get(argv[0].as_str()) {
        Some(func) => func,
        _ => return Err(InternalCommandError::NotFound.into()),
    };

    let mut opened_fds = Vec::new();
    for r in redirects {
        match r.target {
            parser::RedirectionType::FileOrFd(ref target) => {
                let target = expand_word_into_string(shell, target)?;
                if let Ok(fd) = target.parse::<i32>() {
                    match r.fd {
                        0 => stdin = fd,
                        1 => stdout = fd,
                        2 => stderr = fd,
                        _ => (),
                    }
                } else {
                    let options = open_option_new(&r.direction);
                    if let Ok(file) = options.open(&target) {
                        let src = file.into_raw_fd();
                        let dst = r.fd as RawFd;
                        opened_fds.push(src);
                        match dst {
                            0 => stdin = src,
                            1 => stdout = src,
                            2 => stderr = src,
                            _ => (),
                        }
                    } else {
                        print_err!("failed to open file: `{}'", target);
                        return Err(InternalCommandError::BadRedirection.into());
                    }
                }
            }
            parser::RedirectionType::Fd(ref fd) => match r.fd {
                0 => stdin = *fd,
                1 => stdout = *fd,
                2 => stderr = *fd,
                _ => (),
            },
            parser::RedirectionType::File(ref wfilepath) => {
                let options = open_option_new(&r.direction);

                trace!("redirection: options={:?}", options);
                let filepath = expand_word_into_string(shell, wfilepath)?;
                if let Ok(file) = options.open(&filepath) {
                    let src = file.into_raw_fd();
                    let dst = r.fd as RawFd;
                    opened_fds.push(src);
                    match dst {
                        0 => stdin = src,
                        1 => stdout = src,
                        2 => stderr = src,
                        _ => (),
                    }
                } else {
                    print_err!("failed to open file: `{}'", filepath);
                    return Err(InternalCommandError::BadRedirection.into());
                }
            }
            parser::RedirectionType::HereDoc(ref heredoc) => {
                let pipe_out = evaluate_heredoc(shell, heredoc)?;
                match r.fd {
                    0 => stdin = pipe_out,
                    _ => unreachable!(),
                }

                opened_fds.push(pipe_out);
            }
            parser::RedirectionType::UnresolvedHereDoc { .. } => {
                // must be resolved in the parser
                unreachable!()
            }
        }
    }

    let result = func(&mut InternalCommandContext {
        argv,
        shell,
        stdin: FdFile::new(stdin),
        stdout: FdFile::new(stdout),
        stderr: FdFile::new(stderr),
    });

    // Close redirections.
    for fd in opened_fds {
        close(fd).expect("failed to close");
    }

    Ok(result)
}
