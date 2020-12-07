use std::sync::Arc;
use crate::builtins::InternalCommandContext;
use crate::parser;
use crate::process::ExitStatus;
use std::iter::FromIterator;
use std::io::Write;

use pom::parser::*;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Alias {
    pub name: String,
    pub body: String,
}

// name = { (ASCII_ALPHANUMERIC | "_" | "-" | "!")+ }
// body = { "\""? ~ (!"\"" ~ ANY)+ ~ "~\""? }
// alias = { SOI ~ name ~ "=" ~ body ~ EOI }

fn name<'a>() -> Parser<'a, char, String> {
    is_a(|c: char| "_-!".contains(c) || c.is_ascii_alphanumeric()).repeat(1..)
    .map(String::from_iter)
}

fn body<'a>() -> Parser<'a, char, String> {
    (
        (sym('\"') * none_of("\"").repeat(1..) - sym('\"'))
        | none_of(" \t\r\n").repeat(1..)
    ).map(String::from_iter)
}

fn parse<'a>() -> Parser<'a, char, Alias> {
    (name().expect("name") + sym('=').expect("=") * body().expect("body"))
    .map(|(name, body)| {
        Alias {
            name: name,
            body: body,
        }
    })
}


fn parse_alias(alias: &str) -> Result<Alias, parser::ParseError> {
    let chars: Vec<char> = alias.chars().collect();
    parse().parse(Arc::new(InputV { input: chars.to_vec() }))
        .map_err(|err| parser::error_convert(&chars.to_vec(), err))
}

pub fn command(ctx: &mut InternalCommandContext) -> ExitStatus {
    trace!("alias: argv={:?}", ctx.argv);
    if let Some(alias) = ctx.argv.get(1) {
        match parse_alias(alias) {
            Ok(Alias { name, body }) => {
                ctx.shell.add_alias(&name, body);
                ExitStatus::ExitedWith(0)
            }
            Err(parser::ParseError::Expected(err)) |
            Err(parser::ParseError::Fatal(err)) => {
                print_err!("alias: {}", err);
                ExitStatus::ExitedWith(1)
            }
            Err(parser::ParseError::Empty) => {
                print_err!("alias: alias can't be empty string");
                ExitStatus::ExitedWith(1)
            }
        }
    } else {
        // List defined aliases.
        for (name, cmd) in ctx.shell.aliases() {
            writeln!(ctx.stdout, "{}='{}'", name, cmd).ok();
        }

        ExitStatus::ExitedWith(0)
    }
}
