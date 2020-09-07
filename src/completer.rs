use linefeed::complete;
use crate::builtins::INTERNAL_COMMANDS;
use linefeed::complete::{Completer, Suffix};
use linefeed::prompter::Prompter;
use linefeed::terminal::Terminal;
use crate::{utils, shell, variable};

use std::borrow::Cow;
use std::collections::{HashMap, BTreeMap, btree_map};
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
            is_executable(&entry)
        );
    }
}


pub struct CommandScanner {
    /// Key is command name and value is absolute path to the executable.
    paths: Vec<Folder>,
}

impl CommandScanner {
    pub fn new() -> CommandScanner {
        CommandScanner {
            paths: Vec::new(),
        }
    }

    /// Scans bin directories and caches all files in them. Call this method
    /// when you update `$PATH`!
    pub fn scan(&mut self) -> bool {
        let path = utils::var_os("PATH", "");
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

        updated
    }

    pub fn folders(&self) -> &Vec<Folder> {
        &self.paths
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
    mutex_shell: Arc<Mutex<shell::Shell>>,
    folder_scanner: Arc<Mutex<FolderScanner>>,
}

impl ShellCompleter {
    pub fn new(mutex_shell: Arc<Mutex<shell::Shell>>,
        folder_scanner: Arc<Mutex<FolderScanner>>) -> ShellCompleter {
        ShellCompleter {
            mutex_shell: mutex_shell,
            folder_scanner: folder_scanner,
        }
    }

    pub fn complete_command(&self, command: &str) -> Vec<linefeed::Completion> {
        let mut command = String::from(command);
        let mut prefix = "";

        if command.starts_with("`") {
            prefix = "`";
            command.remove(0);
            // command = command.trim_start_matches("`").to_owned();
        }

        // If there is a specific path, look for command in the path.
        let (base_dir, fname) = split_path(&command);
        if let Some(path) = base_dir {
            if let Some(path) = to_path(path) {
                if let Some(folder) = self.folder_scanner.lock().unwrap().scan(&path) {
                    let entries = folder.entries.iter()
                        .filter(|entry| is_executable(entry) || entry.metadata().map_or(false, |meta| meta.is_dir()))
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

        let mut completions = Vec::new();

        let mut shell = self.mutex_shell.lock().unwrap();

        for (key, _value) in shell.commands().filter(&command) {
            if key.starts_with(&command) {
                completions.push(linefeed::Completion {
                    completion: format!("{}{}", prefix, key),
                    display: Some(key.to_string()),
                    suffix: Suffix::Default,
                });
            }
        }

        completions
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
        if let Some(path) = to_path(base_dir.unwrap_or(".")) {
            if let Some(folder) = self.folder_scanner.lock().unwrap().scan(&path) {
                return self.folder_to_completion(&folder.entries, base_dir, fname)
            }
        }

        Vec::new()
    }

    pub fn complete_variable(&self, word: &str) -> Vec<linefeed::Completion> {
        let mut word = String::from(word);
        let mut res = Vec::new();

        let prefix = if word.starts_with("${") {
            "${"
        } else {
            "$"
        };
        word.replace_range(0..prefix.len(), "");

        let shell = self.mutex_shell.lock().unwrap();

        shell.variables().for_each(|(key, v)|
            if let Some(value) = v.value() {
                match value {
                    variable::Value::Function(ref _f) => {}
                    _ => {
                        if key.starts_with(&word) {
                            res.push(linefeed::Completion {
                                completion: format!("{}{}", prefix, key),
                                display: Some(key.to_string()),
                                suffix: Suffix::Default,
                            });
                        }
                    }
                }
            }
        );

        res
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
        } else if word.starts_with("$") {
            self.complete_variable(word)
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

fn is_executable(entry: &fs::DirEntry) -> bool {
    if let Ok(file_type) = entry.file_type() {
        if file_type.is_symlink() {
            // follow symlink and check if it is executable.
            if let Ok(path) = entry.path().canonicalize() {
                if let Ok(meta) = path.metadata() {
                    if (meta.permissions().mode() & 0o111) != 0 {
                        return true;
                    }
                }
            }
        }
    }

    match entry.metadata() {
        Ok(meta) => {
            meta.is_file() && (meta.permissions().mode() & 0o111) != 0
        }
        Err(_) => { false }
    }
}

fn to_path(path: &str) -> Option<PathBuf> {
    let path = PathBuf::from(path);
    if let Ok(striped) = path.strip_prefix(TILDE) {
        utils::home_dir().join(striped).canonicalize().ok()
    } else {
        path.canonicalize().ok()
    }
}

pub enum CommandValue {
    External(Arc<fs::DirEntry>),    // External command
    Builtin,                        // builtin command
    Alias,                          // User defined alias command
    Function,                       // User defined function
}

// Assign int value to CommandValue element to compare the priority.
fn command_value_discriminant_value(command_value: &CommandValue) -> u32 {
    match command_value {
        CommandValue::External(_e) => 0,
        CommandValue::Builtin => 1,
        CommandValue::Alias => 2,
        CommandValue::Function => 3,
    }
}

pub struct CommandMap {
    // BTreeMap is used to cache all commands, because it keeps the order of command.
    commands: BTreeMap<String, CommandValue>,
    // It is necessary for shell parser.
    // A user can define an alias that has same name with external command.
    // Ex) alias ls="ls --color"
    // BTreeMap only keep alias name and remove the external information of "ls".
    // But, shell parser needs to get the external information to execute "ls --color" command.
    external_commands: HashMap<String, Arc<fs::DirEntry>>,
}

impl CommandMap {
    pub fn build(shell: &shell::Shell) -> CommandMap {
        let mut external_commands = HashMap::new();
        let mut commands = BTreeMap::new();

        for path in shell.path_folders().iter().rev() {
            for entry in path.entries.iter() {
                if let Ok(filename) = entry.file_name().into_string() {
                    commands.insert(filename.to_owned(), CommandValue::External(Arc::clone(entry)));
                    external_commands.insert(filename, Arc::clone(entry));
                }
            }
        }

        // Check internal commands
        INTERNAL_COMMANDS.keys().for_each(|key|
            match commands.insert(key.to_string(), CommandValue::Builtin) { _ => () }
        );

        // Check alias commands
        shell.aliases().for_each(|(key, _v)|
            match commands.insert(key.to_string(), CommandValue::Alias) { _ => () }
        );

        // Check user defined functions
        shell.variables().for_each(|(key, v)|
            if let Some(value) = v.value() {
                if let variable::Value::Function(ref _f) = value {
                    match commands.insert(key.to_string(), CommandValue::Function) { _ => () }
                }
            }
        );

        CommandMap {
            commands: commands,
            external_commands: external_commands,
        }
    }

    /// Insert a key and value into BTreeMap.
    /// This function compares the value priority if the Map already has a value.
    pub fn insert(&mut self, key:&str, value: CommandValue) {
        if let Some(has) = self.commands.get(key) {
            if command_value_discriminant_value(&has) >= command_value_discriminant_value(&value) {
                return
            }
        }
        self.commands.insert(key.to_owned(), value);
    }

    pub fn filter(&self, command: &str) -> btree_map::Range<String, CommandValue> {
        self.commands.range(command.to_owned()..)
    }

    pub fn get_external(&self, command: &str) -> Option<&Arc<fs::DirEntry>> {
        self.external_commands.get(command)
    }
}