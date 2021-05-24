use crate::builtins::InternalCommandContext;
use crate::process::ExitStatus;
use crate::utils;
use std::io::Write;
use std::path::PathBuf;

pub fn command(ctx: &mut InternalCommandContext) -> ExitStatus {
    trace!("cd: argv={:?}", ctx.argv);

    let mut old_dir = ctx.shell.get_current_dir();

    let dir = match ctx.argv.get(1) {
        Some(dir) => {
            if dir.starts_with('/') {
                // absolute path
                PathBuf::from(dir)
            } else if dir == "-" {
                match ctx.shell.popd() {
                    Some(dir) => dir,
                    None => return ExitStatus::ExitedWith(0),
                }
            } else if dir == ".." {
                old_dir.pop();
                old_dir
            } else {
                // relative path
                PathBuf::from(&old_dir).join(dir)
            }
        }
        None => {
            // called with no arguments; defaults to the home directory
            utils::home_dir()
        }
    };

    ctx.shell.pushd(dir.clone());

    match ctx.shell.set_current_dir(Some(dir.clone())) {
        Ok(_) => {
            ExitStatus::ExitedWith(0)
        }
        Err(err) => {
            writeln!(ctx.stderr, "rushell: cd: {}: `{}'", err, dir.display()).ok();
            ExitStatus::ExitedWith(1)
        }
    }
}
