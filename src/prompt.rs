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
            user: whoami::username(),
            host: splitted.first().unwrap().to_string(),
            git: git::Git::new(),
            remote_login: env::var_os("SSH_CONNECTION").is_some(),
        }
    }
}

pub trait Prompt {
    fn main(&mut self, shell: &mut shell::Shell, condition: &Condition) -> String;
    fn second(&mut self, _shell: &mut shell::Shell) -> String {
        let style = theme::basic_theme().prompt_continue;
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
        let arrow_style = theme::PromptTheme::arrow(style, next);

        format!("\x01{}\x02\u{E0B0}\x01{}\x02",
            arrow_style.prefix(),
            arrow_style.suffix(),
        )
    }

    fn build(&mut self, _shell: &mut shell::Shell, condition: &Condition, theme: &theme::PromptTheme) -> String {
        let mut cwd = _shell.get_current_dir();
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
                 format!("\x01{}\x02 {} \x01{}\x02", theme.path.prefix(), git_root.display(), theme.path.suffix()),
                 self.arrow_format(theme.path_basename, None))
            };

            format!("\x01{}\x02 \u{E0A0} {} {}{} \x01{}\x02{}{}{}",
                style.prefix(),
                git.branch,
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

        let mut ps1 = parse_ps1(_shell, condition);
        if ps1.is_empty() == false {
            ps1 = format!("\x01{}\x02 {} \x01{}\x02{}",
                theme.repo.prefix(), ps1, theme.repo.suffix(),
                self.arrow_format(theme.repo, Some(theme.path))
            );
        }

        let host = if condition.remote_login {
            format!("\x01{}\x02 {}\x01{}{}\x02@{} \x01{}\x02{}",
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

        let path = format!("\x01{}\x02 {}\x01{}\x02",
            style.prefix(),
            path,
            style.suffix(),
        );

        let style = theme.basename(readonly);

        let base = format!("\x01{}\x02{} \x01{}\x02",
            style.prefix(),
            basename,
            style.suffix(),
        );

        let arrow = self.arrow_format(theme.path_basename, git_style);

        format!("{}{}{}{}{}{}",
            ps1, host, path, base, arrow, git,
        )
    }
}

impl Prompt for PowerLine {
    fn main(&mut self, shell: &mut shell::Shell, condition: &Condition) -> String {
        format!("{} ", self.build(shell, condition, &theme::nord_theme()))
    }

    fn second(&mut self, shell: &mut shell::Shell) -> String {
        let linefeed = shell.linefeed().expect("linefeed is invalid.");
        " ".repeat(linefeed.prompt_len())
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
        let theme = theme::basic_theme();

        let mut cwd = _shell.get_current_dir();
        let readonly = is_readonly(&cwd);

        if let Ok(strip_home) = cwd.strip_prefix(&utils::home_dir()) {
            cwd = PathBuf::from("~").join(strip_home);
        }

        let host_prompt = if condition.remote_login {
            format!("\x01{}\x02{}\x01{}{}\x02@{}\x01{}\x02 ",
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

            format!("on \x01{}\x02\u{E0A0} {} [{}{}]\x01{}\x02",
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
        let style = theme::basic_theme().prompt;

        let mut ps1 = parse_ps1(_shell, condition);
        if ps1.is_empty() == false {
            ps1 = format!("\x01{}\x02{}\x01{}\x02", theme.repo.prefix(), ps1, theme.repo.suffix());
        }

        format!("\n{ps1} {host}\x01{path_prefix}\x02{path}\x01{path_suffix}\x02 {git}\n\x01{prefix}\x02 ❯ \x01{suffix}\x02",
            ps1 = ps1,
            host = host_prompt,
            path_prefix = path_style.prefix(),
            path = path,
            path_suffix = path_style.suffix(),
            git = git_prompt,
            prefix = style.prefix(),
            suffix = style.suffix(),
        )
    }
}

pub struct PromptCommand {
    command: String,
}

impl PromptCommand {
    pub fn new(shell: &shell::Shell) -> Option<PromptCommand> {
        if let Some(command) = shell.get("PROMPT_COMMAND") {
            Some(PromptCommand{
                command: command.as_str().into(),
            })
        } else {
            None
        }
    }
}

impl Prompt for PromptCommand {
    fn main(&mut self, shell: &mut shell::Shell, _condition: &Condition) -> String {
        let (pipe_in, pipe_out) = pipe().expect("failed to create a pipe for PROMPT_COMMAND");
        let _status = shell.run_str_with_stdio(&self.command, false, 0, pipe_out, 2);
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

fn parse_ps1(shell: &mut shell::Shell, condition: &Condition) -> String {
    use chrono::{Local, DateTime};

    let ps1 = shell.get_str("PS1").unwrap_or(String::new());
    let mut res = String::new();

    let mut chars = ps1.chars().peekable();
    while let Some(ch) = chars.next() {
        match ch {
            '\\' => {
                if let Some(ch) = chars.next() {
                    match ch {
                        'd' => {
                            let dt: DateTime<Local> = Local::now();
                            res += &dt.format("%a %h %e").to_string();
                        }
                        't' => {
                            let dt: DateTime<Local> = Local::now();
                            res += &dt.format("%X").to_string();
                        }
                        'T' => {
                            let dt: DateTime<Local> = Local::now();
                            res += &dt.format("%T").to_string();
                        }
                        '@' => {
                            let dt: DateTime<Local> = Local::now();
                            res += &dt.format("%r").to_string();
                        }
                        'A' => {
                            let dt: DateTime<Local> = Local::now();
                            res += &dt.format("%R").to_string();
                        }
                        '[' => {
                            let mut buf = String::new();
                            while let Some(ch) = chars.next() {
                                match chars.peek() {
                                    Some(']') if ch == '\\' => {
                                        chars.next();

                                        if buf.len() > 0 {
                                            res += &format!("\x01{}\x02", buf);
                                            buf = String::new();
                                        }
                                        break;
                                    }
                                    _ => buf.push(ch),
                                }
                            }
                            if buf.len() > 0 {      // If there is no ']', all buffered chars are append "res" as a normal char.
                                res += &buf;
                            }
                        }
                        'H' |
                        'h' => res += &condition.host,
                        'u' => res += &condition.user,
                        's' => res += utils::SHELL_NAME,
                        'n' => res.push('\n'),
                        'r' => res.push('\r'),
                        '\\' => res.push('\\'),
                        'V' | 'v' => res += env!("CARGO_PKG_VERSION"),

                        '$' => if nix::unistd::getuid().is_root() {
                                res.push('#')
                            } else {
                                res.push('$')
                            }

                        // Not supported.
                        // 'e' | 'j' | 'l' | 'w' | 'W' | '!' | '#' | 'D' => {}
                        _ => {
                            res.push('\\');
                            res.push(ch);
                        }
                    }
                } else {
                    res.push('\\')
                }
            }
            '$' => {
                if let Some(ch) = chars.next() {
                    if ch == '(' {
                        let mut buf = String::new();
                        while let Some(ch) = chars.next() {
                            if ch == ')' {
                                let (_, s) = shell.run_to_string(&buf);
                                res += &s;
                                buf = String::new();
                                break;
                            } else {
                                buf.push(ch);
                            }
                        }
                        if buf.len() > 0 {
                            res += &buf;
                        }
                    } else {
                        res.push('$');
                        res.push(ch);
                    }
                } else {
                    res.push('$');
                }
            }
            any => res.push(any),
        }
    }

    res
}
