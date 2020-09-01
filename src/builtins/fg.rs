use crate::builtins::InternalCommandContext;
use crate::process::{continue_job, ExitStatus, Job, JobId};
use std::io::Write;
use std::sync::Arc;
use structopt::StructOpt;

#[derive(Debug, StructOpt)]
#[structopt(name = "fg", about = "fg command.")]
struct Opt {
    #[structopt(name = "job_id")]
    job_id: Option<String>,
}

// Used by bg.
pub(super) fn parse_job_id(
    ctx: &mut InternalCommandContext,
    job_id: Option<String>,
) -> Result<Arc<Job>, ExitStatus> {
    let id = match job_id {
        Some(job_id) => {
            let job_id = job_id.as_str();
            if job_id.chars().nth(0) == Some('%') {
                match (&job_id[1..]).parse() {
                    Ok(job_id) => JobId::new(job_id),
                    Err(_) => {
                        writeln!(ctx.stderr, "rushell: invalid job id `{}'", job_id).ok();
                        return Err(ExitStatus::ExitedWith(1));
                    }
                }
            } else {
                writeln!(ctx.stderr, "rushell: invalid job id `{}'", job_id).ok();
                return Err(ExitStatus::ExitedWith(1));
            }
        }
        None => match ctx.shell.last_fore_job() {
            Some(job) => job.id(),
            None => {
                writeln!(ctx.stderr, "rushell: no jobs to run").ok();
                return Err(ExitStatus::ExitedWith(1));
            }
        },
    };

    match ctx.shell.find_job_by_id(id) {
        Some(job) => Ok(job),
        None => {
            writeln!(ctx.stderr, "rushell: no such job `{}'", id).ok();
            Err(ExitStatus::ExitedWith(1))
        }
    }
}

pub fn command(ctx: &mut InternalCommandContext) -> ExitStatus {
    trace!("fg: argv={:?}", ctx.argv);
    match Opt::from_iter_safe(ctx.argv) {
        Ok(opts) => match parse_job_id(ctx, opts.job_id) {
            Ok(job) => {
                continue_job(ctx.shell, &job, false);
                ExitStatus::ExitedWith(0)
            }
            Err(status) => status,
        },
        Err(err) => {
            writeln!(ctx.stderr, "rushell: fg: {}", err).ok();
            ExitStatus::ExitedWith(1)
        }
    }
}
