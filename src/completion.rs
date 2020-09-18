use crate::{shell};

pub enum ArgOption {
    WordList(String),
    Function(String),
    Action(String),
    Word(String),
}

pub fn split_word_list<'a>(shell: &shell::Shell, list: &'a str) -> Vec<&'a str> {
    list.split(|c| shell.ifs().contains(c)).collect()
}