use linefeed::syntaxer;
use crate::input;
use crate::theme;

pub struct Syntaxer {
    theme: theme::SyntaxTheme,
}

impl Syntaxer {
    pub fn new() -> Syntaxer {
        Syntaxer {
            theme: theme::default_syntax_theme(),
        }
    }
}

impl syntaxer::Syntaxer for Syntaxer {
    fn highlight(&self, buf: &str, pos: usize) -> Option<String> {
        let mut input = input::Input::from(buf);
        input.parse(pos);

        let mut res = String::new();
        let mut copied = pos;

        for word in input.words() {
            let mut start = word.start;

            if start > pos || (start <= pos && pos < word.end) {
                if start <= pos {
                    start = pos;
                }
                if start > copied {
                    res += &buf[copied .. start];
                }

                let style = if let input::Quoted::True(_) = word.quoted {
                    self.theme.quoted
                } else {
                    match word.kind {
                        input::Kind::Command => self.theme.command,
                        input::Kind::Argument => self.theme.argument,
                        input::Kind::ValidPath => self.theme.valid_path,
                        _ => self.theme.normal,
                    }
                };

                res += &format!("\x01{}\x02{}\x01{}\x02",
                    style.prefix(),
                    &buf[start..word.end],
                    style.suffix(),
                );

                copied = word.end;
            }
        }

        if copied < buf.len() {
            res += &buf[copied..];
        }

        if res.is_empty() {
            None
        } else {
            Some(res)
        }
    }
}