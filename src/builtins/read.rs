use crate::builtins::InternalCommandContext;
use crate::process::ExitStatus;
use crate::variable::Value;
use std::io::Write;
use structopt::StructOpt;

use std::sync::{Arc};
use linefeed::{Interface, ReadResult};
use std::sync::atomic::{Ordering};

#[derive(Debug, StructOpt)]
#[structopt(name = "read", about = "read command.")]
struct Opt {
    #[structopt(short = "p")]
    prompt: Option<String>,
    #[structopt(name = "VAR")]
    var_name: String,
}

fn read_line() -> crate::utils::Result<String> {
    let reader = Arc::new(Interface::new("")?);

    reader.bind_sequence("\x1b\x1b[D", linefeed::Command::from_str("backward-word"));
    reader.bind_sequence("\x1b\x1b[C", linefeed::Command::from_str("forward-word"));
    reader.set_report_signal(linefeed::Signal::Interrupt, true);

    // reader.set_prompt(&prompt_display).ok();
    match reader.read_line()? {
        ReadResult::Input(line) => {
            Ok(line.trim().to_string())
        }

        ReadResult::Signal(_sig) => {
            print!("^C");
            std::io::stdout().flush()?;
            crate::eval::CTRLC.store(true, Ordering::Relaxed);
            if _sig == linefeed::Signal::Interrupt {
                reader.cancel_read_line()?;
            }
            Err(Box::new(std::io::Error::new(std::io::ErrorKind::Interrupted, "")))
        }
        ReadResult::Eof => {
            Err(Box::new(std::io::Error::new(std::io::ErrorKind::Interrupted, "")))
        }
    }
}

pub fn command(ctx: &mut InternalCommandContext) -> ExitStatus {
    trace!("read: argv={:?}", ctx.argv);
    match Opt::from_iter_safe(ctx.argv) {
        Ok(opts) => {
            if ctx.shell.interactive() {
                if let Some(prompt) = opts.prompt {
                    write!(ctx.stderr, "{}", prompt).ok();
                    ctx.stderr.flush().ok();
                }
            }

            match read_line() {
                Ok(line) => {
                    let trimed_value = line.trim_end();
                    let value = Value::String(trimed_value.to_owned());
                    ctx.shell.set(&opts.var_name, value, false);
                    ExitStatus::ExitedWith(0)
                }
                Err(_err) => {
                    // EOF
                    ExitStatus::ExitedWith(1)
                }
            }
        }
        Err(err) => {
            writeln!(ctx.stderr, "read: {}", err).ok();
            ExitStatus::ExitedWith(1)
        }
    }
}
