use crate::builtins::InternalCommandContext;
use crate::process::ExitStatus;
use std::io::Write;

pub fn command(ctx: &mut InternalCommandContext) -> ExitStatus {
    if let Some(filepath) = ctx.argv.get(1) {
        match ctx.shell.run_file(std::path::PathBuf::from(&filepath)) {
            Ok(status) => status,
            Err(err) => {
                writeln!(ctx.stderr, "rushell: failed open the file: {}\n{:?}", filepath, err).ok();
                ExitStatus::ExitedWith(1)
            }
        }
    } else {
        writeln!(ctx.stderr, "rushell: source: filename argument required").ok();
        ctx.stderr.flush().ok();
        ExitStatus::ExitedWith(0)
    }
}
