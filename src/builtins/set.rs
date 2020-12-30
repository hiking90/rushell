use crate::builtins::InternalCommandContext;
use crate::process::ExitStatus;
use structopt::StructOpt;

#[derive(Debug, PartialEq, Eq, StructOpt)]
#[structopt(name = "set", about = "Set command.")]
struct Opt {
    #[structopt(short = "e")]
    errexit: bool,
    #[structopt(short = "u")]
    nounset: bool,
    #[structopt(short = "n")]
    noexec: bool,
    #[structopt(long = "--")]
    dashdash: bool,
}

pub fn command(ctx: &mut InternalCommandContext) -> ExitStatus {
    // TODO: Support more options
    // TODO: Support +e, +u, ...

    if ctx.argv.len() >= 2 && ctx.argv[1] == "--" {
        if ctx.argv.len() == 2 {
            let frame = ctx.shell.current_frame_mut();
            for i in 1..10 {
                frame.remove(&i.to_string(), false);
            }
        } else {
            ctx.shell.current_frame_mut().set_args(&ctx.argv[2..]);
        }
        ExitStatus::ExitedWith(0)
    } else {
        match Opt::from_iter_safe(ctx.argv) {
            Ok(opts) => {
                ctx.shell.errexit = opts.errexit;
                ctx.shell.nounset = opts.nounset;
                ctx.shell.noexec = opts.noexec;
                ExitStatus::ExitedWith(0)
            }
            Err(err) => {
                print_err!("set: {}", err);
                ExitStatus::ExitedWith(1)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::shell;
    use crate::process;
    use failure::Error;

    #[test]
    fn test_set_command() -> Result<(), Error> {
        let mut shell = shell::Shell::new();
        process::run_internal_command(
            &mut shell,
            &vec!["set".into(), "--".into(), "1".into(), "2".into(), "3".into()],
            0, 1, 2, &vec![],
        )?;
        assert_eq!(
            shell.current_frame().get_string_args(),
            vec!["1".to_string(), "2".to_string(), "3".to_string()]
        );

        process::run_internal_command(
            &mut shell,
            &vec!["set".into(), "--".into()],
            0, 1, 2, &vec![],
        )?;
        assert_eq!(
            shell.current_frame().get_string_args(),
            Vec::<String>::new()
        );

        Ok(())
    }
}
