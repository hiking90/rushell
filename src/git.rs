use std::process::Command;
use crate::shell::Shell;


pub struct Git {
    branch: String,
    staged: bool,
    unstaged: bool,
    untracked: bool,
}

impl Git {
    pub fn status() -> Result<Git, Box<dyn std::error::Error>> {
        let output = Command::new("git")
            .arg("status")
            .arg("--porcelain")
            .arg("-b")
            .output()?;

        let output = std::str::from_utf8(&output.stdout)?;
        let mut res = Git {
            branch: "".to_owned(),
            staged: false,
            unstaged: false,
            untracked: false,
        };

        for line in output.lines() {
            if line.starts_with("## ") {
                let start = &line[3..];
                if let Some(end) = start.find(".") {
                    res.branch = start[..end].to_owned();
                }
            } else if line.starts_with("?? ") {
                res.untracked = true;
            } else {
                let mut chars = line.chars();
                let staged = chars.next() != Some(' ');
                let unstaged = chars.next() != Some(' ');

                if res.staged == false && staged == true {
                    res.staged = true;
                }
                if res.unstaged == false && unstaged == true {
                    res.unstaged = true;
                }
            }
        }

        Ok(res)
    }
}

pub fn prompt() -> String {
    if let Ok(git) = Git::status() {
        if git.branch.len() != 0 {
            return format!("on  {} [{}{}]",
                git.branch,
                if git.unstaged {"!"} else if git.staged {"+"} else {"*"},
                if git.untracked {"?"} else {""},
            );
        }
    }
    "".to_string()
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