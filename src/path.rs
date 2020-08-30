use linefeed::complete;
use crate::builtins::INTERNAL_COMMANDS;
use linefeed::complete::{Completer, Suffix};
use linefeed::prompter::Prompter;
use linefeed::terminal::Terminal;

use std::borrow::Cow;
use std::collections::HashMap;
use std::path::{Path, PathBuf, is_separator, MAIN_SEPARATOR};
use std::fs;
use std::io;
use std::os::unix::fs::PermissionsExt;
use std::sync::{Arc, Mutex};
use std::time::SystemTime;

#[derive(Debug)]
pub struct Folder {
    path: PathBuf,
    modified: SystemTime,
    entries: Vec<Arc<fs::DirEntry>>,
}

impl Folder {
    fn new(path: &Path) -> io::Result<Folder> {
        Ok(Folder {
            path: path.to_owned(),
            modified: SystemTime::now(),
            entries: Vec::new(),
        })
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
            self.entries.push(Arc::new(entry?));
        }

        Ok(true)
    }

    fn retain_executable(&mut self) {
        self.entries.retain(|entry|
            match entry.metadata() {
                Ok(meta) => {
                    meta.is_file() && (meta.permissions().mode() & 0o111) != 0
                }
                Err(_) => { false }
            }
        );
    }
}

pub type Commands = HashMap<String, Arc<fs::DirEntry>>;

pub struct CommandScanner {
    /// Key is command name and value is absolute path to the executable.
    paths: Vec<Folder>,
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
        let mut paths = Vec::<Folder>::new();
        let mut updated = false;

        for dir in path.split(':') {
            let dir = PathBuf::from(dir);
            if let Some(_idx) = paths.iter().position(|x| x.path == dir) {
                continue;
            }

            let mut folder = if let Some(idx) = self.paths.iter().position(|x| x.path == dir) {
                self.paths.remove(idx)
            } else {
                if let Ok(folder) = Folder::new(&dir) {
                    folder
                } else {
                    continue;
                }
            };

            if folder.update().unwrap_or(false) {
                folder.retain_executable();
                updated = true;
            }

            paths.push(folder);
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
    }

    pub fn commands(&self) -> Arc<Commands> {
        Arc::clone(&self.commands)
    }
}

const FOLDER_CACHE_MAX: usize = 5;
const TILDE: &'static str = "~";

pub struct FolderScanner {
    folders: Vec<Folder>,
}

impl FolderScanner {
    pub fn new() -> FolderScanner {
        FolderScanner {
            folders: Vec::new(),
        }
    }

    pub fn scan(&mut self, path: &Path) -> Option<&Folder> {
        let mut folder = if let Some(idx) = self.folders.iter().position(|f| f.path == path) {
            self.folders.remove(idx)
        } else {
            if let Ok(folder) = Folder::new(&path) {
                folder
            } else {
                return None;
            }
        };

        if let Ok(_updated) = folder.update() {
            if self.folders.len() == FOLDER_CACHE_MAX {
                self.folders.remove(0);
            }
            self.folders.push(folder);
            self.folders.last()
        } else {
            None
        }
    }
}

pub struct ShellCompleter {
    commands_scanner: Arc<Mutex<CommandScanner>>,
    folder_scanner: Arc<Mutex<FolderScanner>>,
    homedir: PathBuf,
}

impl ShellCompleter {
    pub fn new(commands_scanner: Arc<Mutex<CommandScanner>>,
        folder_scanner: Arc<Mutex<FolderScanner>>,
        homedir: &Path) -> ShellCompleter {
        ShellCompleter {
            commands_scanner: commands_scanner,
            folder_scanner: folder_scanner,
            homedir: homedir.to_path_buf(),
        }
    }

    fn to_path(&self, path: &str) -> Option<PathBuf> {
        let path = PathBuf::from(path);
        if let Ok(striped) = path.strip_prefix(TILDE) {
            self.homedir.join(striped).canonicalize().ok()
        } else {
            path.canonicalize().ok()
        }
    }

    pub fn complete_command(&self, command: &str) -> Vec<linefeed::Completion> {
        let mut command = String::from(command);
        let mut prefix = "";

        if command.starts_with("`") {
            prefix = "`";
            command = command.trim_start_matches("`").to_owned();
        }

        let (base_dir, fname) = split_path(&command);
        if let Some(path) = base_dir {
            if let Some(path) = self.to_path(path) {
                if let Some(folder) = self.folder_scanner.lock().unwrap().scan(&path) {
                    let entries = folder.entries.iter()
                        .filter(|entry|
                            match entry.metadata() {
                                Ok(meta) => {
                                    meta.is_dir() || (meta.permissions().mode() & 0o111) != 0
                                }
                                Err(_) => { false }
                            }
                        )
                        // Drop Unix hidden file that is started with "."
                        .filter(|entry| entry.file_name().to_str().map_or(false, |name| !name.starts_with(".")))
                        .map(|entry| entry.clone())
                        .collect();

                    return self.folder_to_completion(&entries, base_dir, fname)
                }
            } else {
                return Vec::new();
            }
        }

        let mut res = Vec::new();

        for key in self.commands_scanner.lock().unwrap().commands().keys() {
            if key.starts_with(&command) &&
                INTERNAL_COMMANDS.get(command.as_str()).is_none() == true {
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
        res
    }

    fn folder_to_completion(&self, entries: &Vec<Arc<fs::DirEntry>>, base_dir: Option<&str>, fname: &str) -> Vec<linefeed::Completion> {
        let mut res = Vec::new();

        for entry in entries {
            let ent_name = entry.file_name();
            if let Ok(path) = ent_name.into_string() {
                if path.starts_with(fname) {
                    let (name, display) = if let Some(dir) = base_dir {
                        (format!("{}{}", dir, path), Some(path))
                    } else {
                        (path, None)
                    };

                    let is_dir = entry.metadata().ok()
                        .map_or(false, |m| m.is_dir());

                    let suffix = if is_dir {
                        Suffix::Some(MAIN_SEPARATOR)
                    } else {
                        Suffix::Default
                    };

                    res.push(linefeed::Completion{
                        completion: name,
                        display: display,
                        suffix: suffix,
                    });
                }
            }
        }

        res
    }

    pub fn complete_folder(&self, path: &str) -> Vec<linefeed::Completion> {
        let (base_dir, fname) = split_path(path);
        if let Some(path) = self.to_path(base_dir.unwrap_or(".")) {
            if let Some(folder) = self.folder_scanner.lock().unwrap().scan(&path) {
                return self.folder_to_completion(&folder.entries, base_dir, fname)
            }
        }

        Vec::new()
    }
}

impl<Term: Terminal> Completer<Term> for ShellCompleter {
    fn complete(
        &self,
        word: &str,
        reader: &Prompter<Term>,
        start: usize,
        _end: usize,
    ) -> Option<Vec<linefeed::Completion>> {
        // println!("{}, {} : {}", word, _start, _reader.buffer());
        if word.starts_with("`") {
            return Some(self.complete_command(word));
        }

        let mut input = String::from(reader.buffer());
        input.truncate(start);
        let input = input.trim();

        let mut res = if input.is_empty() || input.ends_with("&") || input.ends_with("|") ||
            input.ends_with(";") || input.ends_with("`") {
            self.complete_command(word)
        } else {
            self.complete_folder(word)
        };

        res.sort_by(|a, b| a.display().cmp(&b.display()));
        Some(res)
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

fn split_path(path: &str) -> (Option<&str>, &str) {
    match path.rfind(is_separator) {
        Some(pos) => (Some(&path[..pos + 1]), &path[pos + 1..]),
        None => (None, path)
    }
}
