use std::io;
use std::process::Command;

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