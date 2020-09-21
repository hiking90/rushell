use crate::eval::eval;
use crate::linefeed::{DefaultTerminal, Interface};
use crate::parser;
use crate::process::{ExitStatus, Job, JobId, ProcessState};
use crate::variable::{Frame, Value, Variable};
use crate::completer;
use crate::completion::ArgOption;
use nix;
use nix::sys::termios::{tcgetattr, Termios};
use nix::unistd::{getpid, Pid};
use std::collections::{HashMap, HashSet, BTreeMap, hash_map, btree_map};
use std::fmt;
use std::fs::File;
use std::io;
use std::io::prelude::*;
use std::os::unix::io::RawFd;
use std::path::PathBuf;
use std::sync::Arc;

/// A isolated shell execution environment.
pub struct Shell {
    /// The shell's pgid.
    pub shell_pgid: Pid,
    /// Whether the shell is interactive.
    pub interactive: bool,
    /// $0
    pub script_name: String,
    /// A saved terminal state.
    pub shell_termios: Option<Termios>,

    /// `$?`
    last_status: i32,
    /// `$!`
    last_back_job: Option<Arc<Job>>,

    /// Global scope.
    global: Frame,
    /// Local scopes (variables declared with `local').
    frames: Vec<Frame>,
    /// Exported variable names.
    exported: HashSet<String>,
    /// Alias (`alias(1)`).
    aliases: HashMap<String, String>,

    /// `set -e`
    pub errexit: bool,
    /// `set -u`
    pub nounset: bool,
    /// `set -n`
    pub noexec: bool,

    /// Jobs.
    jobs: HashMap<JobId, Arc<Job>>,
    /// Background jobs.
    background_jobs: HashSet<Arc<Job>>,
    /// The current process states spawned by the shell.
    states: HashMap<Pid, ProcessState>,
    /// The mapping from a pid (not job's pgid) to its job.
    pid_job_mapping: HashMap<Pid, Arc<Job>>,
    /// A stack of pathes maintained by pushd(1) / popd(1).
    cd_stack: Vec<String>,

    // TODO: Remove this field or make it private.
    pub last_fore_job: Option<Arc<Job>>,

    linefeed: Option<Arc<Interface<DefaultTerminal>>>,
    commands_scanner: completer::CommandScanner,

    commands: Option<completer::CommandMap>,
    completion: BTreeMap<String, Arc<Vec<ArgOption>>>,
}

impl Shell {
    pub fn new() -> Shell {
        Shell {
            shell_pgid: getpid(),
            script_name: "".to_owned(),
            interactive: false,
            shell_termios: None,
            last_status: 0,
            exported: HashSet::new(),
            aliases: HashMap::new(),
            global: Frame::new(),
            frames: Vec::new(),
            errexit: false,
            nounset: false,
            noexec: false,
            jobs: HashMap::new(),
            background_jobs: HashSet::new(),
            states: HashMap::new(),
            pid_job_mapping: HashMap::new(),
            cd_stack: Vec::new(),
            last_fore_job: None,
            last_back_job: None,
            linefeed: None,
            commands_scanner: completer::CommandScanner::new(),
            commands: None,
            completion: BTreeMap::new(),
        }
    }

    #[cfg(test)]
    pub fn new_for_test() -> Shell {
        Shell::new()
    }

    pub fn set_script_name(&mut self, name: &str) {
        self.script_name = name.to_owned();
    }

    pub fn set_interactive(&mut self, interactive: bool) {
        self.interactive = interactive;
        self.shell_termios = if interactive {
            Some(tcgetattr(0 /* stdin */).expect("failed to tcgetattr"))
        } else {
            None
        };
    }

    pub fn last_status(&self) -> i32 {
        self.last_status
    }

    pub fn set_last_status(&mut self, status: i32) {
        self.last_status = status;
    }

    pub fn last_back_job(&self) -> &Option<Arc<Job>> {
        &self.last_back_job
    }

    pub fn set_last_back_job(&mut self, job: Arc<Job>) {
        self.last_back_job = Some(job);
    }

    #[inline]
    pub fn interactive(&self) -> bool {
        self.interactive
    }

    #[inline]
    pub fn ifs(&self) -> String {
        self.get_str("IFS").unwrap_or_else(|| "\n\t ".to_owned())
    }

    #[inline]
    pub fn enter_frame(&mut self) {
        self.frames.push(Frame::new());
    }

    #[inline]
    pub fn leave_frame(&mut self) {
        self.frames.pop();
    }

    #[inline]
    pub fn in_global_frame(&self) -> bool {
        self.frames.is_empty()
    }

    #[inline]
    pub fn current_frame(&self) -> &Frame {
        self.frames.last().unwrap_or(&self.global)
    }

    #[inline]
    pub fn current_frame_mut(&mut self) -> &mut Frame {
        self.frames.last_mut().unwrap_or(&mut self.global)
    }

    #[inline]
    pub fn assign(&mut self, key: &str, value: Value) {
        let defined_as_local = self.current_frame().get(key).is_some();
        self.set(key, value, defined_as_local);
    }

    pub fn define(&mut self, key: &str, is_local: bool) {
        let frame = if is_local {
            self.current_frame_mut()
        } else {
            &mut self.global
        };

        frame.define(key);
    }

    pub fn set(&mut self, key: &str, value: Value, is_local: bool) {
        let frame = if is_local {
            self.current_frame_mut()
        } else {
            &mut self.global
        };

        frame.set(key, value.clone());

        if let Value::Function(ref _f) = value {
            if let Some(mut commands) = self.commands.take() {
                commands.insert(key, completer::CommandValue::Function);
                self.commands = Some(commands);
            }
        }

        // if is_local == false && key == "PATH" {
        //     // $PATH is being updated. Reload directories.
        //     if let Value::String(ref _path) = value {
        //         self.scan_commands();
        //     }
        // }
    }

    pub fn remove(&mut self, key: &str) -> Option<Arc<Variable>> {
        if let Some(var) = self.current_frame_mut().remove(key) {
            if let Value::Function(ref _f) = var.value().as_ref().unwrap() {
                self.commands = None;
            }
            return Some(var);
        }

        if let Some(var) = self.global.remove(key) {
            if let Value::Function(ref _f) = var.value().as_ref().unwrap() {
                self.commands = None;
            }
            return Some(var);
        }

        None
    }

    pub fn get(&self, key: &str) -> Option<Arc<Variable>> {
        if let Some(var) = self.current_frame().get(key) {
            Some(var)
        } else {
            self.global.get(key)
        }
    }

    #[inline]
    pub fn get_str(&self, key: &str) -> Option<String> {
        match self.get(key) {
            Some(var) => match var.value() {
                Some(Value::String(ref s)) => Some(s.clone()),
                _ => None,
            },
            _ => None,
        }
    }

    pub fn export(&mut self, name: &str) {
        self.exported.insert(name.to_string());
    }

    pub fn exported_names(&self) -> std::collections::hash_set::Iter<String> {
        self.exported.iter()
    }

    pub fn aliases(&self) -> std::collections::hash_map::Iter<String, String> {
        self.aliases.iter()
    }

    pub fn add_alias(&mut self, name: &str, body: String) {
        self.aliases.insert(name.to_string(), body);
        if let Some(mut commands) = self.commands.take() {
            commands.insert(name, completer::CommandValue::Alias);
            self.commands = Some(commands);
        }
    }

    pub fn lookup_alias(&self, alias: &str) -> Option<String> {
        self.aliases.get(&alias.to_string()).cloned()
    }

    pub fn pushd(&mut self, path: String) {
        self.cd_stack.push(path);
    }

    pub fn popd(&mut self) -> Option<String> {
        self.cd_stack.pop()
    }

    pub fn get_var_as_i32(&self, name: &str) -> Option<i32> {
        self.get(name).and_then(|var| match var.value() {
            Some(Value::String(s)) => s.parse().ok(),
            _ => None,
        })
    }

    pub fn commands(&mut self) -> &completer::CommandMap {
        if self.commands.is_none() {
            self.commands = Some(completer::CommandMap::build(self));
        }
        &self.commands.as_ref().unwrap()
    }

    pub fn path_folders(&self) -> &Vec<completer::Folder> {
        self.commands_scanner.folders()
    }

    pub fn jobs(&self) -> &HashMap<JobId, Arc<Job>> {
        &self.jobs
    }

    pub fn jobs_mut(&mut self) -> &mut HashMap<JobId, Arc<Job>> {
        &mut self.jobs
    }

    pub fn background_jobs_mut(&mut self) -> &mut HashSet<Arc<Job>> {
        &mut self.background_jobs
    }

    /// Updates the process state.
    pub fn set_process_state(&mut self, pid: Pid, state: ProcessState) {
        self.states.insert(pid, state);
    }

    /// Returns the process state.
    pub fn get_process_state(&self, pid: Pid) -> Option<&ProcessState> {
        self.states.get(&pid)
    }

    pub fn get_job_by_pid(&self, pid: Pid) -> Option<&Arc<Job>> {
        self.pid_job_mapping.get(&pid)
    }

    fn alloc_job_id(&mut self) -> JobId {
        let mut id = 1;
        while self.jobs.contains_key(&JobId::new(id)) {
            id += 1;
        }

        JobId::new(id)
    }

    pub fn create_job(&mut self, name: String, pgid: Pid, childs: Vec<Pid>) -> Arc<Job> {
        let id = self.alloc_job_id();
        let job = Arc::new(Job::new(id, pgid, name, childs.clone()));
        for child in childs {
            self.set_process_state(child, ProcessState::Running);
            self.pid_job_mapping.insert(child, job.clone());
        }

        self.jobs_mut().insert(id, job.clone());
        job
    }

    #[inline]
    pub fn last_fore_job(&self) -> Option<Arc<Job>> {
        self.last_fore_job.as_ref().cloned()
    }

    pub fn find_job_by_id(&self, id: JobId) -> Option<Arc<Job>> {
        self.jobs.get(&id).cloned()
    }

    /// Parses and runs a shell script file.
    pub fn run_file(&mut self, script_file: PathBuf) -> std::io::Result<ExitStatus> {
        let mut f = File::open(script_file)?;
        let mut script = String::new();
        f.read_to_string(&mut script)?;
        Ok(self.run_str(script.as_str()))
    }

    /// Parses and runs a script. Stdin/stdout/stderr are 0, 1, 2, respectively.
    pub fn run_str(&mut self, script: &str) -> ExitStatus {
        // Inherit shell's stdin/stdout/stderr.
        let stdin = 0;
        let stdout = 1;
        let stderr = 2;
        self.run_str_with_stdio(script, stdin, stdout, stderr)
    }

    /// Parses and runs a script in the given context.
    pub fn run_str_with_stdio(
        &mut self,
        script: &str,
        stdin: RawFd,
        stdout: RawFd,
        stderr: RawFd,
    ) -> ExitStatus {
        match parser::parse(&script) {
            Ok(ast) => eval(self, &ast, stdin, stdout, stderr),
            Err(parser::ParseError::Empty) => {
                // Just ignore.
                ExitStatus::ExitedWith(0)
            }
            Err(parser::ParseError::Expected(_err)) => {
                ExitStatus::Expected
            }
            Err(parser::ParseError::Fatal(err)) => {
                print_err!("parse error: {}", err);
                ExitStatus::ExitedWith(-1)
            }
        }
    }

    pub fn write_fmt(&self, args: fmt::Arguments) -> io::Result<()> {
        if let Some(linefeed) = &self.linefeed {
            linefeed.write_fmt(args)
        } else {
            println!("{}", args);
            Ok(())
        }
    }

    pub fn set_linefeed(&mut self, linefeed: Arc<Interface<DefaultTerminal>>) {
        self.linefeed = Some(linefeed);
    }

    pub fn linefeed(&self) -> Option<Arc<Interface<DefaultTerminal>>> {
        self.linefeed.as_ref().map(|linefeed| Arc::clone(linefeed))
    }

    pub fn scan_commands(&mut self) {
        if self.commands_scanner.scan() == true {
            self.commands = None;
        }
    }

    pub fn variables(&self) -> hash_map::Iter<String, Arc<Variable>> {
        self.global.iter()
    }

    pub fn insert_completion(&mut self, name: String, options: Arc<Vec<ArgOption>>) {
        self.completion.insert(name, options);
    }

    pub fn completion(&self) -> btree_map::Iter<String, Arc<Vec<ArgOption>>> {
        return self.completion.iter();
    }

    pub fn get_completion(&self, command: &str) -> Option<&Arc<Vec<ArgOption>>> {
        self.completion.get(command)
    }
}
