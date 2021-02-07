use crate::builtins::InternalCommandContext;
use crate::process::ExitStatus;
use crate::utils;
use std::io::Write;
use std::path::PathBuf;

pub fn command(ctx: &mut InternalCommandContext) -> ExitStatus {
    trace!("cd: argv={:?}", ctx.argv);
    let old_dir = utils::current_working_dir();
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
            } else {
                // relative path
                PathBuf::from(&old_dir).join(dir)
                // Path::new(&old_dir)
                //     .join(dir.clone())
                //     .to_string_lossy()
                //     .into_owned()
            }
        }
        None => {
            // called with no arguments; defaults to the home directory
            utils::home_dir()
        }
    };

    // TODO: make this configurable
    ctx.shell.pushd(old_dir);

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
