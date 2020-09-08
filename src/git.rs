use std::process::Command;
use crate::shell::Shell;

struct Parser {
    pattern: &'static str,
    parser: Box<dyn Fn(&str, &mut Git)>,
}

impl Parser {
    fn new(pattern: &'static str, parser: Box<dyn Fn(&str, &mut Git)>) -> Parser {
        Parser{
            pattern: pattern,
            parser: parser,
        }
    }
}

pub struct Git {
    pub rootdir: String,
    pub branch: String,
    pub staged: bool,
    pub unstaged: bool,
    pub untracked: bool,
}

impl Git {
    pub fn new() -> Option<Git> {
        let output = Command::new("git")
            .arg("status")
            .arg("--porcelain=2")
            .arg("-b")
            .output().ok()?;

        let status_output = std::str::from_utf8(&output.stdout).ok()?;

        let output = Command::new("git")
            .arg("rev-parse")
            .arg("--show-toplevel")
            .output().ok()?;
        let rootdir_output = std::str::from_utf8(&output.stdout).ok()?;

        let mut res = Git {
            rootdir: "".to_owned(),
            branch: "".to_owned(),
            staged: false,
            unstaged: false,
            untracked: false,
        };

        for line in rootdir_output.lines() {
            let line = line.trim();
            if line.is_empty() == false {
                res.rootdir = line.to_string();
                break;
            }
        }

        let tracked_closure = Box::new(|line: &str, res: &mut Git| {
            let mut chars = line.chars();
            let staged = chars.next() != Some('.');
            let unstaged = chars.next() != Some('.');
            if res.staged == false && staged == true {
                res.staged = true;
            }
            if res.unstaged == false && unstaged == true {
                res.unstaged = true;
            }
        });

        let mut parser = [
            Parser::new("# branch.oid ", Box::new(|line: &str, res: &mut Git| {
                res.branch = format!("({})", line[..8].to_owned());
            })),
            Parser::new("# branch.head ", Box::new(|line: &str, res: &mut Git| {
                if line.starts_with("(") == false {
                    res.branch = line.to_string();
                }
            })),
            Parser::new("? ", Box::new(|_line: &str, res: &mut Git| { res.untracked = true; })),
            Parser::new("1 ", tracked_closure.clone()),
            Parser::new("2 ", tracked_closure),
        ];

        for line in status_output.lines() {
            parser.iter_mut()
                .for_each(|parser| if line.starts_with(parser.pattern) {
                    (parser.parser)(&line[parser.pattern.len()..], &mut res)
                });
        }

        if res.rootdir.is_empty() {
            None
        } else {
            Some(res)
        }
    }
}

pub fn aliases(shell: &mut Shell) {
    let aliases = [
        ("g", "git"),
        ("ga", "git add"),
        ("gaa", "git add --all"),
        ("gapa", "git add --patch"),
        ("gau", "git add --update"),
        ("gav", "git add --verbose"),
        ("gap", "git apply"),
        ("gapt", "git apply --3way"),
        ("gb", "git branch"),
        ("gba", "git branch -a"),
        ("gbd", "git branch -d"),
        ("gd", "git diff"),
        ("gcb", "git checkout -b"),
        ("gcmsg", "git commit -m"),
        ("grm", "git rm"),
        ("grmc", "git rm --cached"),
        ("gsh", "git show"),
        ("gst", "git status"),
        ("gsta", "git stash push"),
        ("gstl", "git stash list"),
        ("gstp", "git stash pop"),
        ("gp", "git push"),
        ("gpf!", "git push --force"),
        ("gupa", "git pull --rebase --autostash"),
        ("gam", "git am"),
        ("gamc", "git am --continue"),
        ("gams", "git am --skip"),
        ("gama", "git am --abort"),
        ("gamscp", "git am --show-current-patch"),
    ];

    for alias in aliases.iter() {
        shell.add_alias(alias.0, alias.1.to_string());
    }

}