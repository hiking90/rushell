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
use std::sync::Arc;

const DEFAULT_PATH: &str = "/sbin:/usr/sbin:/usr/local/sbin:/bin:/usr/bin:/usr/local/bin";

fn main() -> io::Result<()> {
    let interface = Arc::new(Interface::new("rushell")?);

    interface.bind_sequence("\x1b\x1b[D", linefeed::Command::from_str("backward-word"));
    interface.bind_sequence("\x1b\x1b[C", linefeed::Command::from_str("forward-word"));

    let mut shell = shell::Shell::new(interface.clone());

    // Import environment variables.
    for (key, value) in std::env::vars() {
        shell.set(&key, Value::String(value.to_owned()), false);
    }

    if shell.get("PATH").is_none() {
        shell.set("PATH", Value::String(DEFAULT_PATH.to_owned()), false);
    }

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

    let homedir = dirs::home_dir().expect("Cannot read home_dir()");
    let style = Color::Red.bold();
    // let text = ">> ";

    let conf_dir = homedir.join(".config/rushell/");
    let history_file = conf_dir.join("history");

    if fs::create_dir_all(&conf_dir).is_ok() {
        let _ = interface.load_history(&history_file);
    }

    let mut command_scanner = path::CommandScanner::new();

    // The character values '\x01' and '\x02' are used to indicate the beginning
    // and end of an escape sequence. This informs linefeed, which cannot itself
    // interpret the meaning of escape sequences, that these characters are not
    // visible when the prompt is drawn and should not factor into calculating
    // the visible length of the prompt string.

    loop {
        if let Some(path) = env::var_os("PATH") {
            if let Ok(path) = path.into_string() {
                if command_scanner.scan(&path) == true {
                    interface.set_completer(Arc::new(path::ShellCompleter::new(&command_scanner)));
                    shell.set_commands(command_scanner.commands());
                }
            }
        }

        let mut cwd = env::current_dir().expect("Error in current_dir()");
        if let Ok(strip_home) = cwd.strip_prefix(&homedir) {
            cwd = PathBuf::from("~").join(strip_home);
        }

        interface.set_prompt(&format!(
            "\n\x01{prefix}\x02{text}\x01{suffix}\x02\n> ",
            prefix = style.prefix(),
            text = cwd.display(),
            suffix = style.suffix()
        ))?;

        let readline = interface.read_line()?;
        process::check_background_jobs(&mut shell);
        match readline {
            ReadResult::Input(line) => {
                shell.run_str(&line);
                if line.trim().len() != 0 {
                    interface.add_history_unique(line);
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
            )
            .unwrap()
        });
    }

    drop(shell);
    drop(interface);
    drop(command_scanner);

    Ok(())
}
