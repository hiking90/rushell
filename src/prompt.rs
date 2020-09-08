use std::env;
use std::ffi::OsStr;
use std::path::{Path, PathBuf, MAIN_SEPARATOR};
use std::fs::File;
use std::io::Read;
use std::os::unix::io::FromRawFd;
use std::os::unix::fs::MetadataExt;
use whoami;
use crate::{git, theme, shell, utils};
use nix::unistd::{close, pipe, Uid};
use ansi_term::{Style};


pub struct Condition {
    pub prompt: String,
    pub user: String,
    pub host: String,
    pub git: Option<git::Git>,
    pub remote_login: bool,
}

impl Condition {
    pub fn new() -> Condition {
        Condition {
            prompt: utils::var_os("PS1", " ❯ "),
            user: whoami::username(),
            host: whoami::hostname(),
            git: git::Git::new(),
            remote_login: env::var_os("SSH_CONNECTION").is_some(),
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


pub struct PowerLine {
}

impl PowerLine {
    pub fn new() -> PowerLine {
        PowerLine {
        }
    }

    fn arrow_format(&self, style: Style, next: Option<Style>) -> String {
        let arrow_style = theme::Theme::arrow(style, next);

        format!("{arrow_prefix}\u{E0B0}{arrow_suffix}",
            arrow_prefix = arrow_style.prefix(),
            arrow_suffix = arrow_style.suffix(),
        )
    }
}

impl Prompt for PowerLine {
    fn main_display(&mut self, _shell: &mut shell::Shell, condition: &Condition) -> String {
        let theme = theme::nord_theme();

        let mut cwd = utils::current_working_dir();
        let mut git_style = None;

        let readonly = is_readonly(&cwd);

        let git = if let Some(git) = &condition.git {
            let mut git_root = PathBuf::from(&git.rootdir);

            if cwd.starts_with(&git_root) {
                let stripped = cwd.strip_prefix(&git_root).unwrap().to_path_buf();
                cwd = git_root.clone();
                git_root = stripped;
            }

            let style = if git.unstaged {
                theme.repo_dirty
            } else if git.staged {
                theme.repo_staged
            } else {
                theme.repo
            };

            git_style = Some(style);

            let (path_style, path, arrow) = if git_root == PathBuf::new() {
                (None, String::new(), String::new())
            } else {
                (Some(theme.path),
                 format!("{} {} {}", theme.path.prefix(), git_root.display(), theme.path.suffix()),
                 self.arrow_format(theme.path_basename, None))
            };

            format!("{} \u{E0A0} {}{} {}{}{}{}",
                style.prefix(),
                if git.unstaged {"!"} else if git.staged {"+"} else {"*"},
                if git.untracked {"?"} else {""},
                style.suffix(),
                self.arrow_format(style, path_style),
                path,
                arrow,
            )
        } else {
            String::new()
        };

        if let Ok(strip_home) = cwd.strip_prefix(&utils::home_dir()) {
            cwd = PathBuf::from("~").join(strip_home);
        }

        let basename = cwd.file_name().unwrap_or(OsStr::new("")).to_str().unwrap_or("").to_string();
        cwd.pop();

        let mut path = String::new();
        cwd.iter().for_each(|some| {
            path.push(some.to_str().unwrap_or("").chars().next().unwrap_or('?'));
            if path.ends_with(MAIN_SEPARATOR) == false {
                path.push(MAIN_SEPARATOR);
            }
        });

        let style = theme.path(readonly);

        let path = format!("{path_prefix} {path}{path_suffix}",
            path_prefix = style.prefix(),
            path = path,
            path_suffix = style.suffix(),
        );

        let style = theme.basename(readonly);

        let base = format!("{base_prefix}{basename} {base_suffix}",
            base_prefix = style.prefix(),
            basename = basename,
            base_suffix = style.suffix(),
        );

        let arrow = self.arrow_format(theme.path_basename, git_style);

        format!("\x01{}{}{}{}\x02 ",
            path, base, arrow, git,
        )
    }
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

        let mut cwd = utils::current_working_dir();
        let readonly = is_readonly(&cwd);

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

            format!("on \x01{}\u{E0A0} {} [{}{}]{}\x02",
                git_style.prefix(),
                git.branch,
                if git.unstaged {"!"} else if git.staged {"+"} else {"*"},
                if git.untracked {"?"} else {""},
                git_style.suffix(),
            )
        } else {
            "".to_string()
        };

        let mut path = cwd.display().to_string();
        if path.ends_with(MAIN_SEPARATOR) {
            path.pop();
        }
        self.last_prompt = path.clone();
        self.last_prompt.push_str(&git_prompt);

        let path_style = theme.path(readonly);
        let style = theme::default_theme().prompt;

        format!("\n\x01{path_prefix}{path}{path_suffix} {git}\n{prefix}{prompt}{suffix}\x02",
            path_prefix = path_style.prefix(),
            path = path,
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

fn is_readonly(path: &Path) -> bool {
    if let Ok(metadata) = path.metadata() {
        if metadata.uid() == Uid::current().as_raw() {
            return (metadata.mode() & 0o200) == 0;
        } else {
            return (metadata.mode() & 0o022) == 0;
        }
    }

    false
}