use crate::builtins::InternalCommandContext;
use crate::process::ExitStatus;

pub fn command(ctx: &mut InternalCommandContext) -> ExitStatus {
    let linefeed = ctx.shell.linefeed();
    let writer = match linefeed.lock_writer_erase() {
        Ok(writer) => writer,
        Err(err) => {
            write!(ctx.shell, "{}", err);
            return ExitStatus::ExitedWith(255);
        }
    };

    let mut idx = 1;
    match ctx.argv.get(1) {
        Some(pat) => {
            for history in writer.history() {
                if history.find(pat).is_some() {
                    println!("{:5}  {}", idx, history);
                }
                idx += 1;
            }
        }
        None => {
            for history in writer.history() {
                println!("{:5}  {}", idx, history);
                idx += 1;
            }
        }
    };

    ExitStatus::ExitedWith(0)
}
