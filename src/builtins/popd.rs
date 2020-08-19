use crate::builtins::InternalCommandContext;
use crate::process::ExitStatus;
use std::io::Write;
use structopt::StructOpt;

#[derive(Debug, StructOpt)]
#[structopt(name = "popd", about = "Popd command.")]
struct Opt {}

pub fn command(ctx: &mut InternalCommandContext) -> ExitStatus {
    match Opt::from_iter_safe(ctx.argv) {
        Ok(_opts) => match ctx.shell.popd() {
            Some(dir) => match std::env::set_current_dir(&dir) {
                Ok(_) => ExitStatus::ExitedWith(0),
                Err(err) => {
                    writeln!(ctx.stderr, "rushell: popd: {}: `{}'", err, dir).ok();
                    ExitStatus::ExitedWith(1)
                }
            },
            None => {
                writeln!(ctx.stderr, "rushell: popd: directory stack empty").ok();
                ExitStatus::ExitedWith(1)
            }
        },
        Err(err) => {
            writeln!(ctx.stderr, "rushell: popd: {}", err).ok();
            ExitStatus::ExitedWith(1)
        }
    }
}
