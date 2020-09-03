use crate::utils;
use std::env;
use std::path::PathBuf;
use whoami;
use crate::git;
use crate::theme;

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
        let theme = theme::default_theme();
        let path_style = if condition.release_mode == true {
            theme.path[theme::PathColor::ReadWrite as usize]
        } else {
            theme.path[theme::PathColor::ReadOnly as usize]
        };

        let mut cwd = utils::current_working_dir();
        if let Ok(strip_home) = cwd.strip_prefix(&utils::home_dir()) {
            cwd = PathBuf::from("~").join(strip_home);
        }

        let git_prompt = if let Some(git) = &condition.git {
            let git_style = if git.unstaged {
                theme.git[theme::GitColor::Unstaged as usize]
            } else if git.staged {
                theme.git[theme::GitColor::Staged as usize]
            } else {
                theme.git[theme::GitColor::Committed as usize]
            };

            format!("on \x01{} {} [{}{}]{}\x02",
                git_style.prefix(),
                git.branch,
                if git.unstaged {"!"} else if git.staged {"+"} else {"*"},
                if git.untracked {"?"} else {""},
                git_style.suffix(),
            )
        } else {
            "".to_string()
        };

        self.last_prompt = cwd.display().to_string();
        self.last_prompt.push_str(&git_prompt);

        let style = theme.prompt;

        format!("\n\x01{path_prefix}{path}{path_suffix} {git}\n {prefix}>{suffix}\x02 ",
            path_prefix = path_style.prefix(),
            path = cwd.display(),
            path_suffix = path_style.suffix(),
            git = git_prompt,
            prefix = style.prefix(),
            suffix = style.suffix(),
        )
    }

    fn continue_display(&mut self, _condition: &Condition) -> String {
        String::from(">> ")
    }

}