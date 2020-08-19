use crate::builtins::InternalCommandContext;
use crate::process::ExitStatus;
use crate::variable::Value;
use std::io::Write;
use structopt::StructOpt;

#[derive(Debug, StructOpt)]
#[structopt(name = "shift", about = "shift command.")]
struct Opt {
    #[structopt(name = "n")]
    n: Option<usize>,
}

pub fn command(ctx: &mut InternalCommandContext) -> ExitStatus {
    trace!("shift: argv={:?}", ctx.argv);
    match Opt::from_iter_safe(ctx.argv) {
        Ok(opts) => {
            let current = ctx.shell.current_frame_mut();
            let mut args = Vec::new();
            for i in 1.. {
                if let Some(var) = current.get_nth_arg(i) {
                    args.push(var);
                    current.remove_nth_arg(i);
                } else {
                    break;
                }
            }

            for (i, var) in args.iter().skip(opts.n.unwrap_or(1)).enumerate() {
                let value = Value::String(var.as_str().to_string());
                current.set_nth_arg(i + 1, value);
            }

            ExitStatus::ExitedWith(0)
        }
        Err(err) => {
            writeln!(ctx.stderr, "shift: {}", err).ok();
            ExitStatus::ExitedWith(1)
        }
    }
}
