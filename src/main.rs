// #![cfg_attr(test, feature(test))]

#[macro_use]
extern crate lazy_static;
extern crate pest;
#[macro_use]
extern crate pest_derive;
extern crate ansi_term;
extern crate linefeed;
extern crate dirs;
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
mod utils;
mod expand;
mod eval;
mod variable;
mod process;
mod theme;
mod fuzzy;
mod path;
mod shell;


use std::fs;
use std::io;
use std::env;
use std::path::{PathBuf};
use ansi_term::Color;
use linefeed::{Interface, ReadResult};
use crate::variable::Value;
use nix::unistd;
use std::os::unix::io::AsRawFd;
use nix::sys::signal::{sigaction, SaFlags, SigAction, SigHandler, SigSet, Signal};
use std::sync::Arc;

const DEFAULT_PATH: &str = "/sbin:/usr/sbin:/usr/local/sbin:/bin:/usr/bin:/usr/local/bin";

fn main() -> io::Result<()> {
    let interface = Arc::new(Interface::new("rushell")?);

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

    let conf_dir = homedir.join("/.config/rushell/");
    let history_file = conf_dir.join("history");

    if fs::create_dir_all(&conf_dir).is_ok() {
        let _ = interface.load_history(&history_file);
    }

    // The character values '\x01' and '\x02' are used to indicate the beginning
    // and end of an escape sequence. This informs linefeed, which cannot itself
    // interpret the meaning of escape sequences, that these characters are not
    // visible when the prompt is drawn and should not factor into calculating
    // the visible length of the prompt string.

    loop {
        let mut cwd = env::current_dir().expect("Error in current_dir()");
        if let Ok(strip_home) = cwd.strip_prefix(&homedir) {
            cwd = PathBuf::from("~").join(strip_home);
        }

        interface.set_prompt(&format!("\n\x01{prefix}\x02{text}\x01{suffix}\x02\n> ",
            prefix=style.prefix(),
            text=cwd.display(),
            suffix=style.suffix()))?;

        let readline = interface.read_line()?;
        process::check_background_jobs(&mut shell);
        match readline {
            ReadResult::Input(line) => {
                shell.run_str(&line);
                interface.add_history_unique(line);
            }
            _ => break,
        }

        interface.save_history(&history_file);
    }

    Ok(())
}
