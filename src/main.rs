// #![cfg_attr(test, feature(test))]

#[macro_use]
extern crate lazy_static;
extern crate pest;
#[macro_use]
extern crate pest_derive;
extern crate ansi_term;
extern crate dirs;
extern crate linefeed;
#[macro_use]
extern crate log;
extern crate structopt;
#[macro_use]
extern crate failure;
extern crate glob;
extern crate whoami;

// #[cfg(test)]
// extern crate test;

mod parser;
mod pattern;
#[macro_use]
mod macros;
mod builtins;
mod eval;
mod expand;
mod path;
mod process;
mod shell;
mod theme;
mod utils;
mod variable;
mod git;
mod prompt;

// use crate::prompt::Prompt;
use crate::variable::Value;
use linefeed::{Interface, ReadResult};
use nix::sys::signal::{sigaction, SaFlags, SigAction, SigHandler, SigSet, Signal};
use nix::unistd;
use std::fs;
use std::io;
use std::os::unix::io::AsRawFd;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};

const DEFAULT_PATH: &str = "/sbin:/usr/sbin:/usr/local/sbin:/bin:/usr/bin:/usr/local/bin";

fn main() -> io::Result<()> {
    let homedir = utils::home_dir();

    // let command_scanner = Arc::new(Mutex::new(path::CommandScanner::new()));
    let mutex_shell = Arc::new(Mutex::new(shell::Shell::new()));
    // let mut shell = shell::Shell::new(command_scanner.clone());

    // TODO: It should be removed when Rushell will achieve alpha or beta product quality.
    let mut release_mode = false;

    let mut iter = std::env::args();

    iter.next();    // Skip command name.

    if let Ok(mut shell) = mutex_shell.lock() {
        shell.scan_commands();
        while let Some(arg) = iter.next() {
            match arg.as_str() {
                "--release" => release_mode = true,
                "-c" => {
                    let mut command = String::new();
                    while let Some(arg) = iter.next() {
                        command.push_str(&format!(" {}", arg));
                    }
                    let status = match shell.run_str(&command) {
                        process::ExitStatus::ExitedWith(status) => status,
                        _ => 1,
                    };
                    std::process::exit(status);
                }
                _ => {
                    shell.set_script_name(&arg);
                    let status = match shell.run_file(PathBuf::from(arg)) {
                        Ok(process::ExitStatus::ExitedWith(status)) => status,
                        _ => 1,
                    };
                    std::process::exit(status);
                }
            }
        }
    }

    let interface = Arc::new(Interface::new("rushell")?);

    interface.bind_sequence("\x1b\x1b[D", linefeed::Command::from_str("backward-word"));
    interface.bind_sequence("\x1b\x1b[C", linefeed::Command::from_str("forward-word"));

    interface.set_report_signal(linefeed::Signal::Interrupt, true);

    let conf_dir = homedir.join(".config/rushell/");
    let history_file = conf_dir.join("history");
    let init_file = conf_dir.join("init.sh");

    if init_file.exists() == false {
        fs::File::create(&init_file).ok();
    }

    if fs::create_dir_all(&conf_dir).is_ok() {
        let _ = interface.load_history(&history_file);
    }

    let folder_scanner = Arc::new(Mutex::new(path::FolderScanner::new()));

    interface.set_completer(Arc::new(path::ShellCompleter::new(mutex_shell.clone(), folder_scanner.clone())));

    if let Ok(mut shell) = mutex_shell.lock() {
        shell.set_linefeed(interface.clone());

        // Import environment variables.
        for (key, value) in std::env::vars() {
            shell.set(&key, Value::String(value.to_owned()), false);
        }

        if shell.get("PATH").is_none() {
            shell.set("PATH", Value::String(DEFAULT_PATH.to_owned()), false);
        }

        #[cfg(target_os = "linux")]
        shell.run_str("alias ls=\"ls --color\"");

        #[cfg(target_os = "macos")]
        shell.run_str("alias ls=\"ls -Gp\"");

        git::aliases(&mut shell);

        shell.run_file(init_file).ok();

        let stdout = std::fs::File::create("/dev/stdout").unwrap();
        shell.set_interactive(unistd::isatty(stdout.as_raw_fd()).unwrap() /* && opt.command.is_none() && opt.file.is_none() */);
    }

    // Ignore job-control-related signals in order not to stop the shell.
    // (refer https://www.gnu.org/software/libc/manual)
    // Don't ignore SIGCHLD! If you ignore it waitpid(2) returns ECHILD.
    let action = SigAction::new(SigHandler::SigIgn, SaFlags::empty(), SigSet::empty());
    unsafe {
        sigaction(Signal::SIGINT, &action).expect("failed to sigaction");
        sigaction(Signal::SIGQUIT, &action).expect("failed to sigaction");
        sigaction(Signal::SIGTSTP, &action).expect("failed to sigaction");
        sigaction(Signal::SIGTTIN, &action).expect("failed to sigaction");
        sigaction(Signal::SIGTTOU, &action).expect("failed to sigaction");
    }

    let mut prompt = if let Some(prompt) = prompt::PromptCommand::new() {
        Box::new(prompt) as Box<dyn prompt::Prompt>
    } else {
        Box::new(prompt::Default::new()) as Box<dyn prompt::Prompt>
    };
    let mut multiline = String::new();

    loop {
        if let Ok(mut shell) = mutex_shell.lock() {
            shell.scan_commands();
            let prompt_display = if multiline.is_empty() {
                prompt.main_display(&mut shell, &prompt::Condition::new(release_mode))
            } else {
                prompt.continue_display()
            };
            interface.set_prompt(&prompt_display)?;
        }

        let readline = interface.read_line()?;

        if let Ok(mut shell) = mutex_shell.lock() {
            process::check_background_jobs(&mut shell);
            match readline {
                ReadResult::Input(line) => {
                    let trimed = line.trim();

                    if trimed.ends_with("\\") == true {
                        multiline.push_str(&trimed[..trimed.len() - 1]);
                    } else {
                        multiline.push_str(&trimed);
                        shell.run_str(&multiline);
                        multiline = String::new();
                    }

                    if trimed.len() != 0 {
                        interface.add_history_unique(trimed.to_owned());
                    }
                }
                ReadResult::Signal(sig) => {
                    if sig == linefeed::Signal::Interrupt {
                        interface.cancel_read_line()?;
                    }
                    multiline = String::new();
                }
                _ => {
                    writeln!(shell, "BYE BYE!")?;
                    break;
                }
            }

            interface.save_history(&history_file).unwrap_or_else(|err| {
                writeln!(
                    shell,
                    "save_history error: {} {}",
                    history_file.display(),
                    err
                ).unwrap()
            });
        }
    }

    Ok(())
}
