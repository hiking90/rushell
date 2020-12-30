use std::sync::Arc;
use pom::parser::*;

use crate::input_parser::*;

#[derive(Debug, Clone)]
pub struct Word {
    pub start: usize,
    pub end: usize,
    pub kind: Type,
    pub escaped_word: String,
}

impl Word {
    fn new_from_item(item: &Item, escaped: String) -> Word {
        Word {
            start: item.begin,
            end: item.end,
            kind: item.type_of.clone(),
            escaped_word: escaped,
        }
    }

    // fn is_empty(&self) -> bool {
    //     self.start == self.end
    // }
}

pub struct Input<'a> {
    line: &'a str,
    words: Vec<Word>,
}

impl<'a> Input<'a> {
    pub fn from(line: &'a str) -> Input {
        Input {
            line: line,
            words: Vec::new(),
        }
    }

    fn parse_escaped_word(&self, input: &[char]) -> String {
        let mut escaped = String::new();
        let mut iter = input.iter();

        while let Some(ch) = iter.next() {
            match ch {
                '\\' => {
                    if let Some(ch) = iter.next() {
                        escaped.push(*ch);
                    }
                }
                _ => {
                    escaped.push(*ch);
                }
            }
        }

        escaped
    }

    fn traverse_simple_command(&self, item: &Item, cursor: usize, input: &Vec<char>) -> Vec<Word> {
        let mut res_words = vec![];

        if item.type_of == Type::SimpleCommand && item.begin <= cursor && cursor <= item.end {
            for item in &item.children {
                // println!("{:?}", item);
                match item.type_of {
                    Type::Command | Type::Argument => {
                        let mut children_words = vec![];
                        for item in &item.children {
                            let mut words = self.traverse_simple_command(&item, cursor, input);
                            children_words.append(&mut words);
                        }
                        if children_words.len() == 0 {
                            res_words.push(Word::new_from_item(&item, self.parse_escaped_word(&input[item.begin..item.end])))
                        } else {
                            res_words.append(&mut children_words);
                        }
                    }
                    _ => {}
                }
            }
        }

        res_words
    }

    pub fn parse_with_cursor(&mut self, cursor: usize) {
        let input: Vec<char> = self.line.chars().collect();
        match PARSER.parse(Arc::new(InputV { input: input.clone() })) {
            Err(_) => {
                return;
            }

            Ok(items) => {
                for item in items {
                    let mut words = self.traverse_simple_command(&item, cursor, &input);

                    self.words.append(&mut words);
                }
            }
        }

    }

    pub fn words(&self) -> &Vec<Word> {
        &self.words
    }

    pub fn cursor_to_index(&self, cursor: usize) -> usize {
        self.words.iter().position(|w| w.start <= cursor && cursor <= w.end).unwrap_or(self.words.len())
    }

    pub fn line(&self) -> &str {
        self.line
    }
}


#[test]
pub fn test_split_words() {
    let mut input = Input::from("l");
    let mut words = Vec::new();

    input.parse_with_cursor(1);
    input.words().iter().for_each(|w| words.push(w.escaped_word.as_str()));

    assert_eq!(words, ["l"]);

    let mut input = Input::from("\\ls -al   \"hello \'king\' \\\" world\" tail");
    let mut words = Vec::new();

    input.parse_with_cursor(4);
    input.words().iter().for_each(|w| words.push(w.escaped_word.as_str()));

    assert_eq!(words, vec!["ls", "-al", "\"hello \'king\' \" world\"", "tail"]);
    // assert_eq!(input.cursor_to_index(4), 1);

    let mut input = Input::from("ls & cat ; less | function");
    let mut words = Vec::new();

    input.parse_with_cursor(5);
    input.words().iter().for_each(|w| words.push(w.escaped_word.as_str()));

    assert_eq!(words, vec!["cat"]);

    let mut input = Input::from("`ls`");
    let mut words = Vec::new();

    input.parse_with_cursor(1);
    input.words().iter().for_each(|w| words.push(w.escaped_word.as_str()));

    assert_eq!(words, vec!["ls"]);

    // test unicode
    let mut input = Input::from("`우리나라`");
    let mut words = Vec::new();

    input.parse_with_cursor(1);
    input.words().iter().for_each(|w| words.push(w.escaped_word.as_str()));

    assert_eq!(words, vec!["우리나라"]);
    // let mut input = Input::from("ls`cat`less\"function\"");
    // let mut words = Vec::new();

    // input.parse_with_cursor(5);
    // input.words().iter().for_each(|w| words.push(w.escaped_word.as_str()));

    // assert_eq!(words, vec!["ls", "cat", "less", "function"]);
}