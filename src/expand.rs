use crate::eval::*;
use crate::parser::{ExpansionOp, ProcSubstType, Span, Word};
use crate::pattern::{PatternWord};
use crate::shell::Shell;
use crate::variable::Value;
use crate::utils::home_dir_for_user;
use failure::Error;
use std::fs::File;
use std::io::prelude::*;
use std::os::unix::io::FromRawFd;

type Result<I> = std::result::Result<I, Error>;

/// TODO: Aliases should be expanded in the parser in order to support
/// compound lists, e.g. alias cowsay-or-echo="cowsay hi || echo hi".
///
/// That said, I believe this feature is not widely-used feature.
pub fn expand_alias(shell: &Shell, argv: &[Word]) -> Vec<Word> {
    argv
        // Get the first word.
        .get(0)
        // Get the first span in the first word.
        .and_then(|word| word.spans().get(0))
        // Make sure that the span is a literal (not parameters, etc.).
        .and_then(|span| match span {
            Span::Literal(lit) => Some(lit),
            _ => None,
        })
        // The very first span is literal. Search the registered aliases.
        .and_then(|lit| shell.lookup_alias(lit.as_str()))
        .map(|alias_str| {
            // Found the alias. Split the alias string by whitespace into words.
            let mut alias_words: Vec<Word> = alias_str
                .trim()
                .split(' ')
                .map(|w| {
                    let span = Span::Literal(w.to_owned());
                    Word(vec![span])
                })
                .collect();

            // Append argv except the first word (alias name).
            for arg in argv.iter().skip(1) {
                alias_words.push(arg.clone());
            }

            alias_words
        })
        // Failed to expand alias. Return argv as it is.
        .unwrap_or_else(|| argv.to_owned())
}

/// Expands a parameter (`$foo` in e.g. `echo $foo`). It returns `Vec` since
/// `op` can be array expansion. `None` value represents *null*.
pub fn expand_param(
    shell: &mut Shell,
    name: &str,
    op: &ExpansionOp,
) -> Result<Vec<Option<String>>> {
    match name {
        "?" => {
            return Ok(vec![Some(shell.last_status().to_string())]);
        }
        "!" => {
            let pgid = match shell.last_back_job() {
                Some(job) => job.pgid.to_string(),
                None => 0.to_string(),
            };

            return Ok(vec![Some(pgid)]);
        }
        "0" => {
            return Ok(vec![Some(shell.script_name.clone())]);
        }
        "$" => {
            return Ok(vec![Some(shell.shell_pgid.to_string())]);
        }
        "#" => {
            return Ok(vec![Some(shell.current_frame().num_args().to_string())]);
        }
        "*" => {
            let args = shell.current_frame().get_string_args();
            let expanded = args.join(" ");
            return Ok(vec![Some(expanded)]);
        }
        "@" => {
            let args = shell.current_frame().get_string_args();
            return Ok(args.iter().map(|a| Some(a.to_owned())).collect());
        }
        _ => {
            if let Some(var) = shell.get(name) {
                // $<name> is defined.
                let value = var.value();
                match (op, value) {
                    (ExpansionOp::Length, Some(_)) => {     // TODO: Support array length
                        return Ok(vec![Some(var.as_str().len().to_string())]);
                    }
                    (ExpansionOp::Length, None) => {
                        return Ok(vec![Some(0.to_string())]);
                    }
                    (ExpansionOp::GetOrAction(_, _), None) => {
                        return Ok(vec![None]);
                    }
                    (
                        ExpansionOp::Subst {
                            pattern,
                            replacement,
                            op,
                        },
                        Some(_),
                    ) => {
                        let content = var.as_str().to_string();
                        let replaced =
                            crate::pattern::replace_pattern(
                                &PatternWord::new(vec![pattern.to_string()]),
                                &content, replacement,
                                op.unwrap_or(' ') == '/');
                        return Ok(vec![Some(replaced)]);
                    }

                    (ExpansionOp::Prefix(_), _) => {
                        failure::bail!("Unsupported Parameter Expansion of Prefix `{}'", name);
                    }

                    (ExpansionOp::Indices(_), Some(_)) => {
                        failure::bail!("Unsupported Parameter Expansion of Array Indices `{}'", name);
                    }

                    (_, _) => {
                        return Ok(vec![Some(var.as_str().to_string())]);
                    }
                };
            }
        }
    }

    // The variable is not defined or is nulll
    // http://pubs.opengroup.org/onlinepubs/009695399/utilities/xcu_chap02.html#tag_02_06_02
    match op {
        ExpansionOp::Prefix(_) => {
            failure::bail!("Unsupported Parameter Expansion of Prefix `{}'", name);
        }
        ExpansionOp::Indices(_) => {
            Ok(vec![Some("0".to_owned())])
        }
        ExpansionOp::Length => {
            if shell.nounset {
                print_err!("undefined variable `{}'", name);
                std::process::exit(1);
            }

            Ok(vec![Some("0".to_owned())])
        }
        ExpansionOp::GetOrEmpty => {
            if shell.nounset {
                print_err!("undefined variable `{}'", name);
                std::process::exit(1);
            }

            Ok(vec![Some("".to_owned())])
        }
        ExpansionOp::GetOrAction(op, word) => {
            match op.as_str() {
                ":-" | "-" => {
                    expand_word_into_string(shell, word).map(|s| vec![Some(s)])
                }
                ":=" | "=" => {
                    let content = expand_word_into_string(shell, word)?;
                    shell.set(name, Value::String(content.clone()), false);
                    Ok(vec![Some(content)])
                }
                _ => {
                    failure::bail!("unsupported expansion operator `{}'", op)
                }
            }
        }
        // ExpansionOp::GetOrDefault(word) | ExpansionOp::GetNullableOrDefault(word) => {
        //     expand_word_into_string(shell, word).map(|s| vec![Some(s)])
        // }
        // ExpansionOp::GetOrDefaultAndAssign(word)
        // | ExpansionOp::GetNullableOrDefaultAndAssign(word) => {
        //     let content = expand_word_into_string(shell, word)?;
        //     shell.set(name, Value::String(content.clone()), false);
        //     Ok(vec![Some(content)])
        // }
        ExpansionOp::Subst { .. } => Ok(vec![Some("".to_owned())]),
    }
}

fn expand_span_into_vec(shell: &mut Shell, span: &Span) -> Result<(Vec<String>, bool)> {
    match span {
        Span::Literal(s) => Ok((vec![s.clone()], false)),
        Span::Parameter { name, op, quoted: _ } => {
            let (names, _) = expand_span_into_vec(shell, name)?;

            let mut frags = Vec::new();
            for name in names {
                for value in expand_param(shell, &name, op)? {
                    if let Some(frag) = value {
                        frags.push(frag);
                    }
                }
            }

            // (frags, !quoted)
            Ok((frags, false))
        }
        Span::ArrayParameter {
            name,
            index,
            quoted,
        } => {
            let mut result = (vec![], !quoted);
            let idx = expand_span_into_vec(shell, index)?.0.join("");
            if let Ok(idx) = idx.parse::<i32>() {
                if idx < 0 {
                    warn!(
                        "the index must be larger than or equals 0: var={}, index={}",
                        name, idx
                    );
                } else {
                    if let Some(frag) = shell.get(name)
                        .map(|v| v.value_at(idx as usize).to_string()) {
                        result = (vec![frag], !quoted);
                    }
                }
            } else if idx == "*" || idx == "@" {
                if let Some(value) = shell.get(name) {
                    let values = value.array().map_or(vec![], |elems| elems.map(|frag| frag.to_string()).collect());
                    let lit = if *quoted {
                        vec![values.join(" ")]
                    } else {
                        values.into_iter().map(|v| v).collect()
                    };
                    result = (lit, !quoted)
                }
            }
            Ok(result)
        }
        Span::ArithExpr { expr } => {
            let result = evaluate_expr(shell, expr).to_string();
            Ok((vec![result], false))
        }
        Span::Tilde(user) => {
            let home_dir = if let Some(user) = user {
                home_dir_for_user(user).unwrap_or_else(|| format!("~{}", user))
            } else {
                dirs::home_dir().unwrap().to_str().unwrap().to_owned()
            };

            Ok((vec![home_dir], false))
        }
        Span::Command { body, quoted } => {
            let (_, stdout) = eval_in_subshell(shell, body)?;

            let mut raw_stdout = Vec::new();
            unsafe { File::from_raw_fd(stdout).read_to_end(&mut raw_stdout).ok() };

            let output = std::str::from_utf8(&raw_stdout)
                .map_err(|err| {
                    // TODO: support binary output
                    print_err!("binary in variable/expansion is not supported");
                    err
                })?
                .trim_end_matches('\n')
                .to_owned();

            if output.is_empty() {
                Ok((vec![], !quoted))
            } else {
                Ok((vec![output], !quoted))
            }
        }
        Span::ProcSubst { body, subst_type } => {
            let (_, stdout) = eval_in_subshell(shell, body)?;
            match subst_type {
                // <()
                ProcSubstType::StdoutToFile => {
                    let file_name = format!("/dev/fd/{}", stdout);
                    Ok((vec![file_name], false))
                }
                // >()
                ProcSubstType::FileToStdin => {
                    // TODO:
                    unimplemented!();
                }
            }
        }
        Span::SubString { name, offset, length } => {
            failure::bail!("Bash Substring is not supported yet. `{:?}:{:?}:{:?}'", name, offset, length)
        }
    }

}

/// Expands a word int a `Vec`.
pub fn expand_word_into_vec(shell: &mut Shell, word: &Word, ifs: &str) -> Result<Vec<PatternWord>> {
    let mut words = Vec::new();
    let mut current_word = Vec::new();
    for span in word.spans() {
        let (frags, expand) = expand_span_into_vec(shell, span)?;

        // Expand `a${foo}b` into words: `a1` `2` `3b`, where `$foo="1 2 3"`.
        let frags_len = frags.len();
        for frag in frags {
            if expand == true {
                if !current_word.is_empty() {
                    words.push(PatternWord::new(current_word));
                    current_word = Vec::new();
                }

                for word in frag.split(|c| ifs.contains(c)) {
                    words.push(PatternWord::new(vec![word.to_string()]));
                }
            } else {
                current_word.push(frag);
            }

            if frags_len > 1 && !current_word.is_empty() {
                words.push(PatternWord::new(current_word));
                current_word = Vec::new();
            }
        }
    }

    if !current_word.is_empty() {
        words.push(PatternWord::new(current_word));
    }

    trace!("expand_word: word={:?}, to={:?}", word, words);
    // println!("expand_word: word={:?}, to={:?}", word, words);
    if words.is_empty() {
        Ok(vec![])
        // Ok(vec![PatternWord::new(vec![LiteralOrGlob::Literal(
        //     "".into(),
        // )])])
    } else {
        Ok(words)
    }
}

/// Expands a word into a string. Words in a command span `"$(echo foo bar)"` are
/// joined by a whitespace.
pub fn expand_word_into_string(shell: &mut Shell, word: &Word) -> Result<String> {
    let ws: Vec<String> = expand_word_into_vec(shell, word, &shell.ifs())?
        .into_iter()
        .map(|w| w.into_string())
        .collect();

    Ok(ws.join(""))
}

/// Expands words into a `Vec<String>`. A pattern in a word are expanded as a
/// file path globbing.
pub fn expand_words(shell: &mut Shell, words: &[Word]) -> Result<Vec<String>> {
    let mut evaluated = Vec::new();
    for word in words {
        let mut ws = Vec::new();
        for w in expand_word_into_vec(shell, word, &shell.ifs())? {
            for f in w.expand_glob()? {
                ws.push(f);
            }
        }

        evaluated.append(&mut ws);
    }

    Ok(evaluated)
}

/// Expands and merges all pattern words into a single pattern word.
pub fn expand_into_single_pattern_word(shell: &mut Shell, pattern: &Word) -> Result<PatternWord> {
    let mut frags = Vec::new();
    let ifs = ""; /* all whitespaces are treated as a literal */
    for word in expand_word_into_vec(shell, pattern, ifs)? {
        for frag in word.fragments() {
            frags.push(frag.clone());
        }
    }

    Ok(PatternWord::new(frags))
}

// pub fn replace_pattern(
//     shell: &mut Shell,
//     pattern: &str,
//     text: &str,
//     replacement: &str,
//     replace_all: bool,
// ) -> Result<String> {
//     let pat = expand_into_single_pattern_word(shell, pattern)?;
//     let dst = expand_word_into_string(shell, replacement)?;
//     Ok(crate::pattern::replace_pattern(
//         &pat,
//         text,
//         &dst,
//         replace_all,
//     ))
// }
