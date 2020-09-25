use crate::builtins::InternalCommandContext;
use crate::process::ExitStatus;
use std::io::Write;
use structopt::StructOpt;

#[derive(Debug, StructOpt)]
#[structopt(name = "pushd", about = "Pushd command.")]
struct Opt {
    #[structopt(name = "dir")]
    dir: Option<String>,
}

pub fn command(ctx: &mut InternalCommandContext) -> ExitStatus {
    match Opt::from_iter_safe(ctx.argv) {
        Ok(opts) => {
            let dir = if let Some(dir) = opts.dir {
                std::path::PathBuf::from(dir)
            } else {
                std::env::current_dir().expect("failed to getcwd()")
            };

            ctx.shell.pushd(dir);
            ExitStatus::ExitedWith(0)
        }
        Err(err) => {
            writeln!(ctx.stderr, "rushell: pushd: {}", err).ok();
            ExitStatus::ExitedWith(1)
        }
    }
}
