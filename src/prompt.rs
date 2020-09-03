use crate::utils;
use std::env;
use std::path::PathBuf;
use whoami;
use crate::git;
use ansi_term::Color;

pub struct Condition {
    pub user: String,
    pub host: String,
    pub git: Option<git::Git>,
    pub remote_login: bool,
    pub release_mode: bool,
}

impl Condition {
    pub fn new(release_mode: bool) -> Condition {
        Condition {
            user: whoami::username(),
            host: whoami::hostname(),
            git: git::Git::new(),
            remote_login: env::var_os("SSH_CONNECTION").is_some(),
            release_mode: release_mode,
        }
    }
}

pub trait Prompt {
    fn main_display(&mut self, condition: &Condition) -> String;
    fn continue_display(&mut self, condition: &Condition) -> String;
}


pub struct Powerline {

}

pub struct Default {
    last_prompt: String,
}

impl Default {
    pub fn new() -> Default {
        Default {
            last_prompt: "".to_string(),
        }
    }
}

impl Prompt for Default {
    fn main_display(&mut self, condition: &Condition) -> String {
        let style = if condition.release_mode == true {
            Color::Fixed(87).bold()
        } else {
            Color::Red.bold()
        };

        let mut cwd = utils::current_working_dir();
        if let Ok(strip_home) = cwd.strip_prefix(&utils::home_dir()) {
            cwd = PathBuf::from("~").join(strip_home);
        }

        let git_prompt = if let Some(git) = &condition.git {
            format!("on  {} [{}{}]",
                git.branch,
                if git.unstaged {"!"} else if git.staged {"+"} else {"*"},
                if git.untracked {"?"} else {""},
            )
        } else {
            "".to_string()
        };

        self.last_prompt = cwd.display().to_string();
        self.last_prompt.push_str(&git_prompt);

        format!("\n\x01{prefix}\x02{text}\x01{suffix}\x02 {git}\n> ",
            prefix = style.prefix(),
            text = cwd.display(),
            suffix = style.suffix(),
            git = git_prompt)
    }

    fn continue_display(&mut self, _condition: &Condition) -> String {
        String::from("% ")
    }

}