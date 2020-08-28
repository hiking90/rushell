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

use crate::variable::Value;
use ansi_term::Color;
use linefeed::{Interface, ReadResult};
use nix::sys::signal::{sigaction, SaFlags, SigAction, SigHandler, SigSet, Signal};
use nix::unistd;
use std::env;
use std::fs;
use std::io;
use std::os::unix::io::AsRawFd;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};

const DEFAULT_PATH: &str = "/sbin:/usr/sbin:/usr/local/sbin:/bin:/usr/bin:/usr/local/bin";

fn main() -> io::Result<()> {
    let release = std::env::args().nth(1).unwrap_or("debug".to_string()) == "release";

    let interface = Arc::new(Interface::new("rushell")?);

    interface.bind_sequence("\x1b\x1b[D", linefeed::Command::from_str("backward-word"));
    interface.bind_sequence("\x1b\x1b[C", linefeed::Command::from_str("forward-word"));

    let homedir = dirs::home_dir().expect("Cannot read home_dir()");

    let style = if release == true {
        Color::Fixed(87).bold()
    } else {
        Color::Red.bold()
    };

    let conf_dir = homedir.join(".config/rushell/");
    let history_file = conf_dir.join("history");

    if fs::create_dir_all(&conf_dir).is_ok() {
        let _ = interface.load_history(&history_file);
    }

    let command_scanner = Arc::new(Mutex::new(path::CommandScanner::new()));
    let folder_scanner = Arc::new(Mutex::new(path::FolderScanner::new(&homedir)));

    interface.set_completer(Arc::new(path::ShellCompleter::new(command_scanner.clone(), folder_scanner.clone())));

    let mut shell = shell::Shell::new(interface.clone(), command_scanner.clone());

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

    let stdout = std::fs::File::create("/dev/stdout").unwrap();
    shell.set_interactive(unistd::isatty(stdout.as_raw_fd()).unwrap() /* && opt.command.is_none() && opt.file.is_none() */);

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

    loop {
        env::var_os("PATH").map(|path|
            path.into_string().map(|path| command_scanner.lock().unwrap().scan(&path))
        );

        let mut cwd = env::current_dir().expect("Error in current_dir()");
        if let Ok(strip_home) = cwd.strip_prefix(&homedir) {
            cwd = PathBuf::from("~").join(strip_home);
        }

        interface.set_prompt(&format!(
            "\n\x01{prefix}\x02{text}\x01{suffix}\x02 {git}\n> ",
            prefix = style.prefix(),
            text = cwd.display(),
            suffix = style.suffix(),
            git = git::prompt(),
        ))?;

        let readline = interface.read_line()?;
        process::check_background_jobs(&mut shell);
        match readline {
            ReadResult::Input(line) => {
                shell.run_str(&line);
                let trimed = line.trim();
                if trimed.len() != 0 {
                    interface.add_history_unique(trimed.to_owned());
                }
            }
            _ => break,
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

    drop(shell);
    drop(interface);
    drop(command_scanner);

    Ok(())
}
