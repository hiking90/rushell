use std::env;
use std::path::PathBuf;
use std::fs::File;
use std::io::Read;
use whoami;
use crate::{git, theme, shell, utils};
use nix::unistd::{close, pipe};
use std::os::unix::io::FromRawFd;


pub struct Condition {
    pub prompt: String,
    pub user: String,
    pub host: String,
    pub git: Option<git::Git>,
    pub remote_login: bool,
    pub release_mode: bool,
}

impl Condition {
    pub fn new(release_mode: bool) -> Condition {
        Condition {
            prompt: utils::var_os("PS1", " ❯ "),
            user: whoami::username(),
            host: whoami::hostname(),
            git: git::Git::new(),
            remote_login: env::var_os("SSH_CONNECTION").is_some(),
            release_mode: release_mode,
        }
    }
}

pub trait Prompt {
    fn main_display(&mut self, shell: &mut shell::Shell, condition: &Condition) -> String;
    fn continue_display(&mut self) -> String {
        let style = theme::default_theme().prompt_continue;
        format!("\x01{}>>{}\x02 ",
            style.prefix(),
            style.suffix(),
        )
    }
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
    fn main_display(&mut self, _shell: &mut shell::Shell, condition: &Condition) -> String {
        let theme = theme::default_theme();
        let path_style = if condition.release_mode == true {
            theme.path
        } else {
            theme.path_nowrite
        };

        let mut cwd = utils::current_working_dir();
        if let Ok(strip_home) = cwd.strip_prefix(&utils::home_dir()) {
            cwd = PathBuf::from("~").join(strip_home);
        }

        let git_prompt = if let Some(git) = &condition.git {
            let git_style = if git.unstaged {
                theme.repo_dirty
            } else if git.staged {
                theme.repo_staged
            } else {
                theme.repo
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

        let style = theme::default_theme().prompt;

        format!("\n\x01{path_prefix}{path}{path_suffix} {git}\n{prefix}{prompt}{suffix}\x02",
            path_prefix = path_style.prefix(),
            path = cwd.display(),
            path_suffix = path_style.suffix(),
            git = git_prompt,
            prompt = condition.prompt,
            prefix = style.prefix(),
            suffix = style.suffix(),
        )
    }
}

pub struct PromptCommand {
    command: String,
}

impl PromptCommand {
    pub fn new() -> Option<PromptCommand> {
        if let Some(command) = env::var_os("PROMPT_COMMAND") {
            Some(PromptCommand{
                command: command.into_string().ok()?,
            })
        } else {
            None
        }
    }
}

impl Prompt for PromptCommand {
    fn main_display(&mut self, shell: &mut shell::Shell, _condition: &Condition) -> String {
        let (pipe_in, pipe_out) = pipe().expect("failed to create a pipe for PROMPT_COMMAND");
        let _status = shell.run_str_with_stdio(&self.command, 0, pipe_out, 2);
        close(pipe_out).ok();

        let mut prompt = String::new();
        unsafe {
            let mut file = File::from_raw_fd(pipe_in);
            file.read_to_string(&mut prompt).ok();
        }
        prompt
    }
}