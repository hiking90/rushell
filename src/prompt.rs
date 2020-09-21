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
        let hostname = whoami::hostname();
        let splitted: Vec<&str> = hostname.split('.').collect();

        Condition {
            prompt: utils::var_os("PS1", " ❯ "),
            user: whoami::username(),
            host: splitted.first().unwrap().to_string(),
            git: git::Git::new(),
            remote_login: env::var_os("SSH_CONNECTION").is_some(),
        }
    }
}

pub trait Prompt {
    fn main(&mut self, shell: &mut shell::Shell, condition: &Condition) -> String;
    fn second(&mut self, _shell: &mut shell::Shell, _condition: &Condition) -> String {
        let style = theme::default_theme().prompt_continue;
        format!("\x01{} ❯{}\x02 ",
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

        format!("{}\u{E0B0}{}",
            arrow_style.prefix(),
            arrow_style.suffix(),
        )
    }

    fn build(&mut self, _shell: &mut shell::Shell, condition: &Condition, theme: &theme::Theme) -> String {
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

        let host = if condition.remote_login {
            format!("{} {}{}{}@{} {}{}",
                theme.username.prefix(),
                condition.user,
                theme.username.suffix(),
                theme.hostname.prefix(),
                condition.host,
                theme.hostname.suffix(),
                self.arrow_format(theme.hostname, Some(theme.path)),
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

        let path = format!("{} {}{}",
            style.prefix(),
            path,
            style.suffix(),
        );

        let style = theme.basename(readonly);

        let base = format!("{}{} {}",
            style.prefix(),
            basename,
            style.suffix(),
        );

        let arrow = self.arrow_format(theme.path_basename, git_style);

        format!("{}{}{}{}{}",
            host, path, base, arrow, git,
        )
    }
}

impl Prompt for PowerLine {
    fn main(&mut self, shell: &mut shell::Shell, condition: &Condition) -> String {
        format!("\x01{}\x02 ", self.build(shell, condition, &theme::nord_theme()))
    }

    fn second(&mut self, shell: &mut shell::Shell, condition: &Condition) -> String {
        let normal = self.build(shell, condition, &theme::plain_theme());
        " ".repeat(normal.chars().count() + 1)
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
    fn main(&mut self, _shell: &mut shell::Shell, condition: &Condition) -> String {
        let theme = theme::default_theme();

        let mut cwd = utils::current_working_dir();
        let readonly = is_readonly(&cwd);

        if let Ok(strip_home) = cwd.strip_prefix(&utils::home_dir()) {
            cwd = PathBuf::from("~").join(strip_home);
        }

        let host_prompt = if condition.remote_login {
            format!("{}{}{}{}@{}{} ",
                theme.username.prefix(), condition.user, theme.username.suffix(),
                theme.hostname.prefix(), condition.host, theme.hostname.suffix(),
            )
        } else {
            String::new()
        };

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

        format!("\n \x01{host}{path_prefix}{path}{path_suffix} {git}\n{prefix}{prompt}{suffix}\x02",
            host = host_prompt,
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
    fn main(&mut self, shell: &mut shell::Shell, _condition: &Condition) -> String {
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