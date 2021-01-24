use crate::builtins::InternalCommandContext;
use crate::process::ExitStatus;
use crate::completion::{ArgOption, split_word_list};
use std::io::Write;
use std::sync::Arc;

fn parse_args(ctx: &mut InternalCommandContext) -> Option<(Arc<Vec<ArgOption>>, Vec<String>)> {
    let mut options = Vec::new();
    let mut iter = ctx.argv.iter();
    let mut names = Vec::<String>::new();

    iter.next();
    while let Some(arg) = iter.next() {
        match arg {
            _ if "-W" == arg => {
                if let Some(arg) = iter.next() {
                    options.push(ArgOption::WordList(arg.into()));
                }
            }
            _ if "-F" == arg => {
                if let Some(arg) = iter.next() {
                    options.push(ArgOption::Function(arg.into()));
                }
            }
            _ if "--" == arg => {
                let word = if let Some(arg) = iter.next() {
                    arg.to_owned()
                } else {
                    "".to_owned()
                };
                options.push(ArgOption::Word(word));
            }
            _ if arg.starts_with("-") == false => {
                names.push(arg.into());
            }
            _ => {
                writeln!(ctx.stderr, "{} does not support {} option.", ctx.argv[0], arg).ok();
            }
        }
    }

    Some((Arc::new(options), names))
}

pub fn compgen(ctx: &mut InternalCommandContext) -> ExitStatus {
    trace!("compgen: argv={:?}", ctx.argv);

    if let Some((args, _names)) = parse_args(ctx) {
        let mut words = Vec::<String>::new();

        args.iter().for_each(|arg| match arg {
            ArgOption::WordList(list) => {
                split_word_list(ctx.shell, list).iter().for_each(|v| words.push(v.to_string()));
            }
            ArgOption::Function(_func) => {
                println!("compgen: -F option is not supported.");
            }
            ArgOption::Word(word) => {
                words.iter().for_each(|w| if w.starts_with(word) {
                    writeln!(ctx.stdout, "{}", w).ok();
                });
            }
            // _ => unreachable!(),
        });

        ExitStatus::ExitedWith(0)
    } else {
        ExitStatus::ExitedWith(1)
    }

}

pub fn complete(ctx: &mut InternalCommandContext) -> ExitStatus {
    trace!("complete: argv={:?}", ctx.argv);

    if let Some((args, names)) = parse_args(ctx) {
        if names.is_empty() {
            for (key, value) in ctx.shell.completion() {
                let mut option_str = String::new();
                for option in value.iter() {
                    match option {
                        ArgOption::WordList(list) => {
                            option_str.push_str(&format!("-W '{}'", list));
                        }
                        ArgOption::Function(func) => {
                            option_str.push_str(&format!("-F '{}'", func));
                        }
                        _ => unreachable!(),
                    }
                }
                writeln!(ctx.stdout, "complete {} {}", option_str, key).ok();
            }
        } else {
            for name in names {
                ctx.shell.insert_completion(name, args.clone());
            }
        }
        ExitStatus::ExitedWith(0)
    } else {
        ExitStatus::ExitedWith(1)
    }
}
