use crate::program::ast::Error;

pub type Spanned<'input> = (usize, Tok<'input>, usize);

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Tok<'input> {
    Text(&'input str),
    DoubleQuotedText(&'input str),
    AndIf,
    OrIf,
    Amp,
    Semi,
    Or,
    DoubleQuoted,
    SingleQuoted,
    Backtick,
    CommandSpan,
    LExprSpan,
    RExprSpan,
    LParen,
    RParen,
    LBracket,
    RBracket,
    LDBracket,
    RDBracket,
    Dollar,
    Equals,
    EqualsPlus,
    EqualsTilde,
    DEquals,
    Backslash,
    Linefeed,
    LBrace,
    RBrace,
    ParamEx,
    Bang,
    BangEquals,
    Space,
    Star,
    AtSign,
    Eof,

    Less(Option<char>),
    DLess(Option<char>),
    DLessDash(Option<char>),
    LessAnd(Option<char>),
    Great(Option<char>),
    GreatAnd(Option<char>),
    DGreat(Option<char>),
    LessGreat(Option<char>),
    Clobber(Option<char>),
    AndGreat(Option<char>),
    AndDGreat(Option<char>),

    DashOp(char),
    DashOp2(&'input str),

    If,
    Then,
    Fi,
    Elif,
    Else,

    While,
    Do,
    Done,
}

use std::str::CharIndices;

pub struct Lexer<'input> {
    chars: std::iter::Peekable<CharIndices<'input>>,
    input: &'input str,
    stack: Vec<Spanned<'input>>,
    next_tok: Option<Spanned<'input>>,
    text: Option<Spanned<'input>>,
}

impl<'input> Lexer<'input> {

    pub fn new(input: &'input str) -> Self {
        Lexer {
            chars: input.char_indices().peekable(),
            input: input,
            stack: Vec::new(),
            next_tok: None,
            text: None,
        }
    }

    fn is_quoted(&self) -> bool {
        match self.stack.last() {
            Some(peek) if peek.1 == Tok::DoubleQuoted => true,
            _ => false,
        }
    }

    fn is_double_bracket(&self) -> bool {
        match self.stack.last() {
            Some(peek) if peek.1 == Tok::LDBracket => true,
            _ => false,
        }
    }

    fn is_pair_tok(&mut self, item: Spanned<'input>) -> bool {
        if let Some(peek) = self.stack.last() {
            match item.1 {
                Tok::DoubleQuoted | Tok::SingleQuoted | Tok::Backtick => {
                    if peek.1 == item.1 {
                        self.stack.pop();
                    } else {
                        self.stack.push(item);
                    }
                }
                Tok::LExprSpan | Tok::CommandSpan | Tok::ParamEx | Tok::LBracket | Tok::LDBracket => {
                    self.stack.push(item);
                }
                Tok::RExprSpan => {
                    if peek.1 == Tok::LExprSpan {
                        self.stack.pop();
                    } else {
                        return false;
                    }
                }
                Tok::RParen => {
                    if peek.1 == Tok::CommandSpan || peek.1 == Tok::LParen {
                        self.stack.pop();
                    } else {
                        return false;
                    }
                }
                Tok::RBrace => {
                    if peek.1 == Tok::ParamEx {
                        self.stack.pop();
                    } else {
                        return false;
                    }
                }
                Tok::RBracket => {
                    if peek.1 == Tok::LBracket {
                        self.stack.pop();
                    } else {
                        return false;
                    }
                }
                Tok::RDBracket => {
                    if peek.1 == Tok::LDBracket {
                        self.stack.pop();
                    } else {
                        return false;
                    }
                }
                _ => return false,
            }
        } else {
            match item.1 {
                Tok::RExprSpan | Tok::RParen | Tok::RBrace | Tok::RBracket | Tok::RDBracket => return false,
                _ => self.stack.push(item),
            }
        }

        true
    }

    fn parse_redirect(&mut self, item: (usize, char), n: Option<char>) -> (Tok<'input>, usize) {
        match item {
            (j, '<') => {
                match self.chars.peek() {
                    Some((j, '<')) => {
                        let j = *j;
                        self.chars.next();
                        match self.chars.peek() {
                            Some((j, '-')) => {
                                let j = *j;
                                self.chars.next();
                                (Tok::DLessDash(None), j + 1)
                            }
                            _ => (Tok::DLess(None), j + 1),
                        }
                    }
                    Some((j, '&')) => {
                        let j = *j;
                        self.chars.next();
                        (Tok::LessAnd(n), j + 1)
                    }
                    _ => (Tok::Less(n), j + 1),
                }
            }
            (j, '>') => {
                match self.chars.peek() {
                    Some((j, '>')) => {
                        let j = *j;
                        self.chars.next();
                        (Tok::DGreat(n), j + 1)
                    }
                    Some((j, '&')) => {
                        let j = *j;
                        self.chars.next();
                        (Tok::GreatAnd(n), j + 1)
                    }
                    Some((j, '|')) => {
                        let j = *j;
                        self.chars.next();
                        (Tok::Clobber(n), j + 1)
                    }
                    _ => (Tok::Great(n), j + 1)
                }
            }
            _ => unreachable!(),
        }
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Result<Spanned<'input>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(next) = self.next_tok {
            if next.1 == Tok::Eof {
                return None;
            } else {
                self.next_tok = None;
                return Some(Ok(next));
            }
        }

        loop {
            let quoted = self.is_quoted();
            let item = match self.chars.next() {
                None => (0, Tok::Eof, 0), // End of file

                Some((i, n)) if n >= '0' && n <= '9' && !quoted => {
                    match self.chars.peek() {
                        Some((j, ch)) if *ch == '<' || *ch == '>' => {
                            let j = *j;
                            let ch = *ch;
                            self.chars.next();
                            let res = self.parse_redirect((j, ch), Some(n));
                            (i, res.0, res.1)
                        }
                        _ => (i, Tok::Text(&self.input[i..i+1]), i + 1)
                    }
                }

                Some((i, '\n')) if !quoted => (i, Tok::Linefeed, i + 1),

                Some((i, ' ')) | Some((i, '\t')) if !quoted => (i, Tok::Space, i + 1),

                Some((i, ch)) if !quoted && (ch == '<' || ch == '>') => {
                    let res = self.parse_redirect((i, ch), None);
                    (i, res.0, res.1)
                }

                Some((i, '*')) if !quoted => (i, Tok::Star, i + 1),
                Some((i, '@')) if !quoted => (i, Tok::AtSign, i + 1),
                Some((i, ';')) if !quoted => (i, Tok::Semi, i + 1),
                Some((i, '&')) if !quoted => {
                    match self.chars.peek() {
                        Some((j, '&')) => {
                            let j = *j;
                            self.chars.next();
                            (i, Tok::AndIf, j + 1)
                        }
                        Some((j, '>')) => {
                            let j = *j;
                            self.chars.next();
                            match self.chars.peek() {
                                Some((j, '>')) => {
                                    let j = *j;
                                    self.chars.next();
                                    (i, Tok::AndDGreat(None), j + 1)
                                }
                                _ => (i, Tok::AndGreat(None), j + 1)
                            }
                        }
                        _ => (i, Tok::Amp, i + 1),
                    }
                }
                Some((i, '|')) if !quoted => {
                    match self.chars.peek() {
                        Some((j, '|')) => {
                            let j = *j;
                            self.chars.next();
                            (i, Tok::OrIf, j + 1)
                        }
                        _ => (i, Tok::Or, i + 1),
                    }
                }
                Some((i, '=')) if !quoted => {
                    match self.chars.peek() {
                        Some((_j, '+')) => {
                            self.chars.next();
                            (i, Tok::EqualsPlus, i + 2)
                        }
                        Some((_j, '=')) => {
                            self.chars.next();
                            (i, Tok::DEquals, i + 2)
                        }
                        Some((_j, '~')) => {
                            self.chars.next();
                            (i, Tok::EqualsTilde, i + 2)
                        }
                        _ => (i, Tok::Equals, i + 1),
                    }
                }
                Some((i, '\\')) => {
                    match self.chars.peek() {
                        Some((_, '\n')) => {
                            self.chars.next();
                            continue;
                        }
                        Some((_, '\\')) | Some((_, '\"')) | Some((_, '\'')) | Some((_, '|')) |
                        Some((_, '&')) | Some((_, ';')) | Some((_, '<')) | Some((_, '>')) | Some((_, '(')) | Some((_, ')')) |
                        Some((_, '$')) | Some((_, '`')) | Some((_, ' ')) | Some((_, '\t')) | Some((_, '=')) | Some((_, '!')) |
                        Some((_, '{')) | Some((_, '}'))
                        => {
                            self.chars.next();
                            (i, Tok::Text(&self.input[i..i+2]), i + 2)
                        }
                        _ => (i, Tok::Backslash, i + 1),
                    }
                }
                Some((i, '!')) if !quoted => {
                    match self.chars.peek() {
                        Some((_, '=')) => {
                            self.chars.next();
                            (i, Tok::BangEquals, i + 2)
                        }
                        _ => (i, Tok::Bang, i + 1),
                    }
                }

                Some((i, '\"')) => {
                    let tok = (i, Tok::DoubleQuoted, i + 1);

                    if self.is_pair_tok(tok) {
                        tok
                    } else {
                        (i, Tok::Text(&self.input[i..i+1]), i + 1)
                    }
                }

                Some((i, '\'')) => {
                    let tok = (i, Tok::SingleQuoted, i + 1);
                    if self.is_pair_tok(tok) {
                        tok
                    } else {
                        (i, Tok::Text(&self.input[i..i+1]), i + 1)
                    }
                }

                Some((i, '`')) => {
                    let tok = (i, Tok::Backtick, i + 1);
                    if self.is_pair_tok(tok) {
                        tok
                    } else {
                        (i, Tok::Text(&self.input[i..i+1]), i + 1)
                    }
                }

                Some((i, '[')) => {
                    let tok = match self.chars.peek() {
                        Some((j, '[')) => {
                            let j = *j;
                            self.chars.next();
                            (i, Tok::LDBracket, j + 1)
                        }
                        _ => (i, Tok::LBracket, i + 1),
                    };
                    if self.is_pair_tok(tok) {
                        tok
                    } else {
                        (i, Tok::Text(&self.input[i..i+1]), i + 1)
                    }
                }

                Some((i, ']')) => {
                    let tok = match self.chars.peek() {
                        Some((j, ']')) => {
                            let j = *j;
                            self.chars.next();
                            (i, Tok::RDBracket, j + 1)
                        }
                        _ => (i, Tok::RBracket, i + 1),
                    };
                    if self.is_pair_tok(tok) {
                        tok
                    } else {
                        (i, Tok::Text(&self.input[i..i+1]), i + 1)
                    }
                }

                Some((i, '}')) => {
                    let tok = (i, Tok::RBrace, i + 1);
                    if self.is_pair_tok(tok) {
                        tok
                    } else {
                        (i, Tok::Text(&self.input[i..i+1]), i + 1)
                    }
                }

                Some((i, ')')) => {
                    let tok = match self.chars.peek() {
                        Some((j, ')')) => {
                            let res = (i, Tok::RExprSpan, *j + 1);
                            self.chars.next();
                            res
                        }
                        _ => (i, Tok::RParen, i + 1),
                    };

                    if self.is_pair_tok(tok) {
                        tok
                    } else {
                        (i, Tok::Text(&self.input[i..i+1]), i + 1)
                    }
                }

                Some((i, '$')) => {
                    let tok = match self.chars.peek() {
                        Some((j, '(')) => {
                            let j = *j;
                            self.chars.next();
                            match self.chars.peek() {
                                Some((k, '(')) => {
                                    let k = *k;
                                    self.chars.next();
                                    (i, Tok::LExprSpan, k + 1)
                                }
                                _ => (i, Tok::CommandSpan, j + 1),
                            }
                        }
                        Some((j, '{')) => {
                            let j = *j;
                            self.chars.next();
                            (i, Tok::ParamEx, j + 1)
                        }
                        _ => (i, Tok::Dollar, i + 1),
                    };

                    if self.is_pair_tok(tok) {
                        tok
                    } else {
                        (i, Tok::Text(&self.input[i..i+1]), i + 1)
                    }
                }

                Some((i, ch)) => {
                    let end = i + ch.len_utf8();
                    (i, Tok::Text(&self.input[i..end]), end)
                }
            };

            match item.1 {
                Tok::Text(_) => {
                    self.text = match self.text {
                        Some(mut text) => {
                            text.1 = Tok::Text(&self.input[text.0..item.2]);
                            text.2 = item.2;
                            Some(text)
                        }
                        _ => Some(item),
                    };
                }
                // Tok::Space => if let Some(text) = self.text {
                //     self.text = None;
                //     return Some(Ok(text));
                // }
                _ => {
                    // Skip tail spaces.
                    let item = if !quoted {
                        let mut end = item.2;
                        while let Some((j, ch)) = self.chars.peek() {
                            if *ch == ' ' || *ch == '\t' || (*ch == '\n' && item.1 == Tok::Linefeed) {
                                end = *j + 1;
                                self.chars.next();
                            } else {
                                break;
                            }
                        }
                        (item.0, item.1, end)
                    } else {
                        item
                    };

                    return match self.text {
                        Some(text) => {
                            if item.1 == Tok::Space {
                                self.next_tok = None;
                            } else {
                                self.next_tok = Some(item);
                            }
                            self.text = None;

                            if !quoted {
                                let mut ignore_line_feed = true;
                                let res = match text.1 {
                                    Tok::Text(term) if term == "if" => Some(Ok((text.0, Tok::If, text.2))),
                                    Tok::Text(term) if term == "else" => Some(Ok((text.0, Tok::Else, text.2))),
                                    Tok::Text(term) if term == "then" => Some(Ok((text.0, Tok::Then, text.2))),
                                    Tok::Text(term) if term == "elif" => Some(Ok((text.0, Tok::Elif, text.2))),
                                    Tok::Text(term) if term == "fi" => Some(Ok((text.0, Tok::Fi, text.2))),

                                    Tok::Text(term) if term == "while" => Some(Ok((text.0, Tok::While, text.2))),
                                    Tok::Text(term) if term == "do" => Some(Ok((text.0, Tok::Do, text.2))),
                                    Tok::Text(term) if term == "done" => Some(Ok((text.0, Tok::Done, text.2))),

                                    _ => {
                                        ignore_line_feed = false;
                                        Some(Ok(text))
                                    }
                                };

                                if ignore_line_feed && item.1 == Tok::Linefeed {
                                    self.next_tok = None;
                                }

                                res
                            } else if self.is_double_bracket() {
                                match text.1 {
                                    Tok::Text(term) if
                                        term == "-z" || term == "-n" || term == "-a" || term == "-b" ||
                                        term == "-c" || term == "-d" || term == "-e" || term == "-f" ||
                                        term == "-g" || term == "-h" || term == "-k" || term == "-p" ||
                                        term == "-r" || term == "-s" || term == "-t" || term == "-u" ||
                                        term == "-w" || term == "-x" || term == "-O" || term == "-G" ||
                                        term == "-L" || term == "-N" || term == "-S"
                                        => Some(Ok((text.0, Tok::DashOp(term.chars().nth(1).unwrap()), text.2))),

                                    Tok::Text(term) if
                                        term == "-gt" || term == "-lt" || term == "-ge" ||
                                        term == "-le" || term == "-eq" || term == "-ne" ||
                                        term == "-nt" || term == "-ot" || term == "-ef"
                                        => Some(Ok((text.0, Tok::DashOp2(&self.input[text.0 .. text.2]), text.2))),

                                    _ => Some(Ok(text))
                                }
                            } else {
                                Some(Ok(text))
                            }
                        }
                        _ => if item.1 == Tok::Eof {
                            None
                        } else {
                            Some(Ok(item))
                        }
                    };
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
}