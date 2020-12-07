use std::sync::Arc;
use std::path::{Path, PathBuf, MAIN_SEPARATOR};
use std::fs;
use std::iter::FromIterator;

use regex::Regex;
use pom::parser::*;

// use pest::Parser;
// use pest::iterators::Pair;

// #[derive(Parser)]
// #[grammar = "glob.pest"]
// struct GlobParser;

// match_char_any = @{ (!"]" ~ ANY) }
// // [abcd-f[:alpha:]]
// match_char_span = { "[" ~ match_char_span_inner* ~ "]"}
// match_char_span_inner = _{
//     match_char_class |
//     match_char_range |
//     match_char_not |
//     match_char_any
// }
// match_char_class = @{ "[:" ~ ASCII_ALPHANUMERIC+ ~ ":]" }
// match_char_range = { match_char_any ~ "-" ~ match_char_any }
// match_char_not = @{ ("!" | "^") ~ match_char_any }
fn match_char_span<'a>() -> Parser<'a, char, String> {

    sym('[') *
    (
        (tag("[:") * is_a(|c: char| c.is_ascii_alphanumeric()).repeat(1..) - tag(":]"))
        .map(|chars| format!("[:{}:]", String::from_iter(chars)))

        | (none_of("]") - sym('-') + none_of("]"))
        .map(|(ch1, ch2)| format!("{}-{}", ch1, ch2))

        | (one_of("!^") * none_of("]"))
        .map(|ch| format!("^{}", ch))

        | none_of("]")
        .map(|ch| ch.to_string())
    ).repeat(0..)
    .map(|strs| format!("[{}]", strs.join("")))

    - sym(']')
}

// match_string_any = _{ !("}" | ",") ~ ANY }
// // {abc,def}
// match_string_span = { "{" ~ match_string_span_inner* ~ "}" }
// match_string_span_inner = _{
//     match_string_word ~ ("," ~ match_string_span_inner)*
// }
// match_string_word = { match_string_any+ }

fn match_string_span<'a>() -> Parser<'a, char, String> {
    sym('{') *
    (
        none_of("},").repeat(1..).map(String::from_iter)
        + (sym(',') * none_of("},").repeat(1..).map(String::from_iter)).repeat(0..)
    ).map(|(span, spans)| {
        let mut res = String::new();
        res.push_str(&span);
        if spans.len() > 0 {
            res.push('|');
            res.push_str(&spans.join("|"));
        }
        format!("({})", res)
    })
    - sym('}')
}

// // !(a|b) ?(a|b)
// extglob = ${ extglob_op ~ "(" ~ pattern_list ~ ")" }
// extglob_op = { "?" | "*" | "+" | "@" | "!" }
// pattern_list = { pattern ~ ("|" ~ pattern_list)* }
// pattern = _{ (!("|" | ")") ~ ANY)* }
fn extglob<'a>() -> Parser<'a, char, String> {
    (
        one_of("?*+@!")
        - sym('(')
        + (
            none_of("|)").repeat(1..).map(String::from_iter)
            + (sym('|') * none_of("|)").repeat(1..).map(String::from_iter)).repeat(0..)
        )
        - sym(')')
    ).map(|(op, (pattern, patterns))| {
        let mut res = String::new();
        res.push_str(&pattern);
        if patterns.len() > 0 {
            res.push('|');
            res.push_str(&patterns.join("|"));
        }
        format!("{}({})", op, res)
    })
}

// glob_span = {
//     (
//     match_char_span |
//     match_string_span |
//     extglob |
//     double_any_string |
//     any_string |
//     any_char |
//     escaped_char |
//     unescaped_char
//     )+
// }
fn glob_span<'a>() -> Parser<'a, char, String> {
    (
        match_char_span()
        | match_string_span()
        | extglob()
        | tag("**").map(|_| ".*".to_string())
        | one_of("*?").map(|ch| if ch == '*' {
                format!("[^{}]*", MAIN_SEPARATOR)
            } else {
                ".".to_string()
            }
        )
                    // '\\' | '.' | '[' | ']' | '(' | ')' | '+' | '?' | '*' | '|' | '{' | '}' | '^' | '$'=> {
        | ((sym('\\') * any()) | any()).map(|ch| {
            if "\\.[]()+?*|{}^$".contains(ch) {
                format!("\\{}", ch)
            } else {
                ch.to_string()
            }
        })
    ).repeat(1..)
    .map(|strs| strs.join(""))
}

// path_span = { "/"* ~ glob_span ~ ("/"+ ~ glob_span)* }
fn path_span<'a>() -> Parser<'a, char, Vec<String>> {
    (sym('/').opt() * glob_span() + (sym('/') * glob_span()).repeat(0..))
    .map(|(span, mut spans)| {
        let mut res = vec![span];
        res.append(&mut spans);
        res
    })
}



// fn parse_glob_span(pair: Pair<Rule>) -> String {
//     let mut regex = String::new();

//     for pair in pair.into_inner() {
//         match pair.as_rule() {
//             Rule::match_char_span => {
//                 regex += "[";
//                 for pair in pair.into_inner() {
//                     match pair.as_rule() {
//                         Rule::match_char_class => {
//                             regex += pair.as_str();
//                         }
//                         Rule::match_char_range => {
//                             regex += pair.as_str();
//                         }
//                         Rule::match_char_not => {
//                             regex += &pair.as_str().replace("!", "^");
//                         }
//                         Rule::match_char_any => {
//                             regex += pair.as_str();
//                         }
//                         _ => unreachable!(),
//                     }
//                 }
//                 regex += "]";
//             }
//             Rule::match_string_span => {
//                 let mut add_filter = false;
//                 regex += "(";
//                 for pair in pair.into_inner() {
//                     match pair.as_rule() {
//                         Rule::match_string_word => {
//                             if add_filter == true {
//                                 regex += "|";
//                             }
//                             regex += pair.as_str();
//                             add_filter = true;
//                         }
//                         _ => unreachable!(),
//                     }
//                 }
//                 regex += ")";
//             }
//             Rule::extglob => {

//             }
//             Rule::double_any_string => {
//                 regex += ".*";
//             }
//             Rule::any_string => {
//                 regex += &format!("[^{}]*", MAIN_SEPARATOR);
//             }
//             Rule::any_char => {
//                 regex += "."
//             }
//             Rule::escaped_char |
//             Rule::unescaped_char => {
//                 let mut chars = pair.as_str().chars();
//                 if pair.as_rule() == Rule::escaped_char {
//                     chars.next().unwrap();  // Skip '\\' char
//                 }
//                 let ch = chars.next().unwrap();
//                 match ch {
//                     '\\' | '.' | '[' | ']' | '(' | ')' | '+' | '?' | '*' | '|' | '{' | '}' | '^' | '$'=> {
//                         regex.push('\\');
//                         regex.push(ch);
//                     }
//                     _ => regex.push(ch),
//                 }
//             }
//             _ => unreachable!(),
//         }
//     }

//     // println!("Regex : {:?}", regex);
//     regex
// }

// fn parse_path_span(pair: Pair<Rule>) -> Vec<String> {
//     let mut globs = Vec::new();

//     for pair in pair.into_inner() {
//         match pair.as_rule() {
//             Rule::glob_span => {
//                 globs.push(parse_glob_span(pair))
//             }
//             _ => unreachable!(),
//         }
//     }

//     globs
// }

fn parse_glob(glob: &str) -> Option<Vec<String>> {
    let input: Vec<char> = glob.chars().collect();
    path_span().parse(Arc::new(InputV { input: input.to_vec() })).ok()
    // let mut pairs = GlobParser::parse(Rule::glob, glob).ok()?;
    // Some(parse_path_span(pairs.next().unwrap()))
}

// pub fn includes_glob(glob: &str) -> bool {
//     GlobParser::parse(Rule::glob, glob).map_or_else(|_| false, |mut pairs|
//         pairs.next().unwrap().into_inner().next().map_or_else(|| false, |pair| {
//             for pair in pair.into_inner() {
//                 match pair.as_rule() {
//                     Rule::match_char_span |
//                     Rule::match_string_span |
//                     Rule::extglob |
//                     Rule::double_any_string |
//                     Rule::any_string |
//                     Rule::any_char => {
//                         return true;
//                     }
//                     _ => {}
//                 }
//             }
//             false
//         })
//     )
// }

fn visit_dirs(path: &Path, strip: bool, depth: usize, file_regex: &Regex, dir_regex: &Regex) -> Option<Vec<String>> {
    let mut paths = Vec::new();

    for entry in fs::read_dir(path).ok()? {
        let entry = entry.ok()?;
        let path = entry.path();

        if path.file_name()?.to_str()?.starts_with(".") {
            continue;
        }

        let path = if strip == true {
            path.strip_prefix("./").unwrap().to_path_buf()
        } else {
            path.to_path_buf()
        };

        let path_str = path.to_str()?;
        if let Some(matched) = file_regex.find(path_str) {
            paths.push(path_str[matched.range()].to_owned());
        }

        if path.is_dir() {
            // println!("subdir {}", path.display());
            if depth != 0 && dir_regex.is_match(path.to_str()?) {
                if let Some(mut res) = visit_dirs(&path, false, depth - 1, file_regex, dir_regex) {
                    paths.append(&mut res);
                }
            }
        }
    }

    Some(paths)
}

pub fn glob_to_regex(glob: &str, match_all: bool) -> Option<(Regex, Regex)> {
    match parse_glob(&glob) {
        Some(mut patterns) => {
            println!("glob_to_regex - {:?}", patterns);
            let last_pattern = patterns.pop()?;
            let mut dir_pattern = String::new();

            for idx in 0..patterns.len() {
                if idx != 0 {
                    dir_pattern += "/+";
                }
                dir_pattern += &patterns[idx];
            }
            let mut file_pattern = if dir_pattern.len() == 0 {
                last_pattern
            } else {
                format!("{}/+{}", dir_pattern, last_pattern)
            };

            if match_all == true {
                file_pattern.push('$');
                dir_pattern.push('$');
            }
            // println!("glob_to_regex - {} {}", dir_pattern, file_pattern);
            Some((Regex::new(&dir_pattern).ok()?, Regex::new(&file_pattern).ok()?))
        }
        None => None,
    }
}

pub fn glob(glob: &str) -> Option<Vec<String>> {
    let path = PathBuf::from(&glob);
    let regexs = glob_to_regex(&glob, true);
    if path.exists() || regexs.is_none() {
        return Some(vec![path.to_str()?.to_owned()]);
    }

    let (dir_regex, file_regex) = regexs?;

    let parent = path.parent().map_or_else(|| Path::new(""), |p| p.into());
    let available = path.ancestors().find(|a| a.exists());

    let (strip, depth, root_dir) = match available {
        Some(available) => {
            (false, parent.components().count() - available.components().count(), available)
        }
        None => (true, parent.components().count(), Path::new(".")),
    };

    // println!("dir \"{:?}\", file \"{:?}\", depth: {}, root_dir: {:?}, strip: {}", dir_regex, file_regex, depth, root_dir, strip);
    visit_dirs(root_dir, strip, depth, &file_regex, &dir_regex)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::process::Command;

    fn compare_glob(pattern: &str) {
        let mut res = glob(pattern).unwrap();
        res.sort();

        let output = Command::new("/bin/sh")
            .arg("-c")
            .arg(format!("echo {}", pattern))
            .output().unwrap();
        let output = String::from_utf8(output.stdout).unwrap();

        let mut sh_res: Vec<&str> = output.trim().split(' ').collect();
        sh_res.sort();

        assert_eq!(res, sh_res);
    }

    #[test]
    fn test_parse_glob() {
        compare_glob("*/*");
        compare_glob("src/*.rs");
        compare_glob("**/*.rs");
        compare_glob("src/**/*.rs");
        compare_glob("src/*.{rs,pest}");
        compare_glob("src/s*.?s");
        compare_glob("src/[[:alnum:]]*.[^p]?");     // "src/[[:alnum:]][^/]*\\.[^p]."
    }

    // #[test]
    // fn test_include_glob() {
    //     assert_eq!(includes_glob("src/*.rs"), true);
    //     assert_eq!(includes_glob("src/[[:alnum:]]*.[^p]?"), true);
    //     assert_eq!(includes_glob("\\[\\*src.rs"), false);
    // }
}