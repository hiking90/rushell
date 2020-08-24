use linefeed::complete;
use crate::builtins::INTERNAL_COMMANDS;

use linefeed::complete::{Completer, Suffix};
use linefeed::prompter::Prompter;
use linefeed::terminal::Terminal;
use std::borrow::Cow;
use std::collections::HashMap;

use std::fs;
use std::io;
use std::os::unix::fs::PermissionsExt;
use std::sync::Arc;
use std::time::SystemTime;

#[derive(Debug)]
struct Apath {
    path: String,
    modified: SystemTime,
    entries: Vec<Arc<fs::DirEntry>>,
}

impl Apath {
    fn new(path: &str) -> Self {
        Apath {
            path: path.to_owned(),
            modified: SystemTime::now(),
            entries: Vec::new(),
        }
    }

    fn update(&mut self) -> Result<bool, io::Error> {
        let meta = fs::metadata(&self.path)?;
        let modified = meta.modified()?;

        if modified == self.modified {
            return Ok(false);
        }

        self.modified = modified;
        self.entries.clear();

        for entry in fs::read_dir(&self.path)? {
            let entry = entry?;
            let meta = entry.metadata()?;
            let mode = meta.permissions().mode();
            if meta.is_file() && (mode & 0o111) != 0 {
                self.entries.push(Arc::new(entry));
            }
        }

        Ok(true)
    }
}

pub type Commands = HashMap<String, Arc<fs::DirEntry>>;

pub struct CommandScanner {
    /// Key is command name and value is absolute path to the executable.
    paths: Vec<Apath>,
    commands: Arc<Commands>,
}

impl CommandScanner {
    pub fn new() -> CommandScanner {
        CommandScanner {
            paths: Vec::new(),
            commands: Arc::new(Commands::new()),
        }
    }

    /// Scans bin directories and caches all files in them. Call this method
    /// when you update `$PATH`!
    pub fn scan(&mut self, path: &str) -> bool {
        let mut paths = Vec::new();
        let mut updated = false;

        for dir in path.split(':') {
            if let Some(idx) = self.paths.iter().position(|x| x.path == dir) {
                let mut apath = self.paths.remove(idx);
                if apath.update().unwrap_or(false) {
                    updated = true;
                }
                paths.push(apath);
                continue;
            } else if let Some(_idx) = paths.iter().position(|x| x.path == dir) {
                continue;
            }

            let mut apath = Apath::new(dir);
            if apath.update().unwrap_or(false) {
                updated = true;
            }

            paths.push(apath);
        }

        self.paths = paths;

        if updated == true {
            let mut commands = Commands::new();

            for path in self.paths.iter().rev() {
                for entry in path.entries.iter() {
                    if let Ok(filename) = entry.file_name().into_string() {
                        commands.insert(filename, Arc::clone(entry));
                    }
                }
            }

            self.commands = Arc::new(commands);
        }

        updated
        // println!("Scan : {:?}", self.paths);
    }

    pub fn commands(&self) -> Arc<HashMap<String, Arc<fs::DirEntry>>> {
        Arc::clone(&self.commands)
    }
}

pub struct ShellCompleter {
    commands: Arc<HashMap<String, Arc<fs::DirEntry>>>,
}

impl ShellCompleter {
    pub fn new(scanner: &CommandScanner) -> ShellCompleter {
        ShellCompleter {
            commands: scanner.commands(),
        }
    }

    pub fn complete_command(&self, command: &str) -> Vec<linefeed::Completion> {
        let mut res = Vec::new();
        let mut command = String::from(command);
        let mut prefix = "";

        if command.starts_with("`") {
            prefix = "`";
            command = command.trim_start_matches("`").to_owned();
        }

        for key in self.commands.keys() {
            if key.starts_with(&command) && INTERNAL_COMMANDS.get(command.as_str()).is_none() == true {
                res.push(linefeed::Completion {
                    completion: format!("{}{}", prefix, key),
                    display: Some(key.to_owned()),
                    suffix: Suffix::Default,
                });
            }
        }

        for key in INTERNAL_COMMANDS.keys() {
            if key.starts_with(&command) {
                res.push(linefeed::Completion {
                    completion: format!("{}{}", prefix, key),
                    display: Some(key.to_string()),
                    suffix: Suffix::Default,
                });
            }
        }
        res.sort_by(|a, b| a.display().cmp(&b.display()));
        res
    }
}

impl<Term: Terminal> Completer<Term> for ShellCompleter {
    fn complete(
        &self,
        word: &str,
        _reader: &Prompter<Term>,
        _start: usize,
        _end: usize,
    ) -> Option<Vec<linefeed::Completion>> {
        // println!("{}, {} : {}", word, _start, _reader.buffer());
        if word.starts_with("`") {
            return Some(self.complete_command(word));
        }

        let mut input = String::from(_reader.buffer());
        input.truncate(_start);
        let input = input.trim();

        if input.is_empty() || input.ends_with("&") || input.ends_with("|") ||
            input.ends_with(";") || input.ends_with("`") {
            Some(self.complete_command(word))
        } else {
            Some(complete::complete_path(word))
        }
    }

    fn word_start(&self, line: &str, end: usize, _reader: &Prompter<Term>) -> usize {
        complete::escaped_word_start(&line[..end])
    }

    fn quote<'b>(&self, word: &'b str) -> Cow<'b, str> {
        complete::escape(word)
    }

    fn unquote<'b>(&self, word: &'b str) -> Cow<'b, str> {
        complete::unescape(word)
    }
}
