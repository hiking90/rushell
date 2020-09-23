const SPACES: &str = " \t\n";
const QUOTE_CHARS: &str = "\"'`";
pub const ESCAPED_CHARS: &str = "\"\'`$\\";
// const PART_BREAK_CHARS: &str = "@$><=;|&{(/";

#[derive(Debug, Clone)]
pub enum Quoted {
    False,
    True(char),
    Progress(char),
}

#[derive(Debug, Clone)]
pub struct Word {
    pub start: usize,
    pub end: usize,
    pub quoted: Quoted,
    pub escaped_word: String,
    escaped: bool,
}

impl Word {
    fn new() -> Word {
        Word {
            start: 0,
            end: 0,
            quoted: Quoted::False,
            escaped: false,
            escaped_word: String::new(),
        }
    }

    fn is_empty(&self) -> bool {
        self.start == self.end
    }

    // pub fn is_progress(&self) -> bool {
    //     match self.quoted {
    //         Quoted::Progress(_) => true,
    //         _ => self.escaped,
    //     }
    // }

    fn push(&mut self, idx: usize, ch: char) -> bool {
        if self.escaped == true {
            self.escaped = false;
            if ESCAPED_CHARS.contains(ch) == false {
                self.escaped_word.push('\\');
            }
            self.escaped_word.push(ch);
        } else if ch == '\\' {
            self.escaped = true;
        } else {
            if QUOTE_CHARS.contains(ch) {
                match self.quoted {
                    Quoted::False => {
                        if self.is_empty() == false {
                            return true;
                        }
                        self.quoted = Quoted::Progress(ch);
                    }
                    Quoted::Progress(qch) => {
                        if qch == ch {
                            self.quoted = Quoted::True(ch);
                            self.end = idx + 1;
                            return true;
                        } else {
                            self.escaped_word.push(ch);
                        }
                    }
                    _ => unreachable!(),
                }
            } else if SPACES.contains(ch) == true {
                match self.quoted {
                    Quoted::False => {
                        return self.is_empty() == false;
                    }
                    Quoted::Progress(_) => self.escaped_word.push(ch),
                    _ => unreachable!(),
                }
            } else {
                self.escaped_word.push(ch);
            }
        }

        if self.is_empty() {
            self.start = idx;
        }
        self.end = idx + 1;

        false
    }
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

    fn parse_words(&mut self) {
        let mut word = Word::new();

        for (idx, ch) in self.line.char_indices() {
            if word.push(idx, ch) == true {
                if word.is_empty() == false {
                    let word_end = word.end;
                    self.words.push(word);
                    word = Word::new();
                    if word_end == idx {
                        word.push(idx, ch);
                    }
                }
            }
        }

        if word.is_empty() == false {
            self.words.push(word);
        }
    }

    pub fn parse(&mut self, cursor: usize) {
        self.parse_words();

        let mut found = false;
        let mut start: usize = 0;
        let mut end: usize = 0;

        for i in 0 .. self.words.len() {
            if start == end {
                start = i;
            }
            end = i + 1;

            let word = &self.words[i];

            if word.start <= cursor && cursor <= word.end {
                found = true;
            }

            if let Quoted::False = word.quoted {
                if word.escaped_word.ends_with(&['&', '|', ';'][..]) {
                    if found == true {
                        break;
                    } else {
                        start = end;
                    }
                }
            }
        }
        self.words = self.words[start .. end].to_vec();
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
    let mut input = Input::from("\\ls -al   \"hello \'king\' \\\" world\" tail");
    let mut words = Vec::new();

    input.parse(4);
    input.words().iter().for_each(|w| words.push(w.escaped_word.as_str()));

    assert_eq!(words, vec!["\\ls", "-al", "hello \'king\' \" world", "tail"]);
    // assert_eq!(input.cursor_to_index(4), 1);

    let mut input = Input::from("ls & cat ; less | function");
    let mut words = Vec::new();

    input.parse(5);
    input.words().iter().for_each(|w| words.push(w.escaped_word.as_str()));

    assert_eq!(words, vec!["cat", ";"]);

    let mut input = Input::from("ls`cat`less\"function\"");
    let mut words = Vec::new();

    input.parse(5);
    input.words().iter().for_each(|w| words.push(w.escaped_word.as_str()));

    assert_eq!(words, vec!["ls", "cat", "less", "function"]);
}