use std::sync::Arc;
use linefeed::syntaxer;
use pom::parser::*;
use std::iter::FromIterator;

use crate::theme;
use crate::input_parser::*;

pub struct Syntaxer {
    theme: theme::SyntaxTheme,
    // parser: Parser<'static, char, Vec<Item>>,
}

impl Syntaxer {
    pub fn new() -> Syntaxer {
        Syntaxer {
            theme: theme::default_syntax_theme(),
            // parser: complete_commands(),
        }
    }

    // fn traverse_children<'a>(&self, item: &'a Item, pos: usize) -> &'a Item {
    //     for child in &item.children {
    //         if child.begin <= pos && pos < child.end {
    //             return self.traverse_children(&child, pos);
    //         }
    //     }

    //     item
    // }
}

impl syntaxer::Syntaxer for Syntaxer {
    fn highlight(&self, buf: &str, pos: usize) -> Option<(String, usize)> {
        let buf: Vec<char> = buf.chars().collect();
        let items = PARSER.parse(Arc::new(InputV { input: buf.clone() })).ok()?;

        // println!("{:?}", items);

        let items = items.into_iter().filter(|item| pos <= item.end).collect::<Vec<Item>>();

        let mut res = String::new();

        let mut start = pos;
        let mut pos = pos;

        for item in items {
            if item.begin > pos {
                res += &String::from_iter(&buf[pos .. item.begin]);
                pos = item.begin;
            } else if item.begin < pos {
                pos = item.begin;
                if pos < start {
                    start = pos;
                }
            }

            for item in item.children {
                let style = match item.type_of {
                    Type::Command => self.theme.command,
                    Type::Argument => self.theme.argument,
                    // Type::Path => self.theme.valid_path,
                    Type::Literal |
                    Type::Assignment |
                    Type::Redirect |
                    Type::Quoted |
                    Type::Symbol |
                    Type::Variable |
                    Type::Initializer |
                    Type::Redirection |
                    Type::RedirectionTarget => self.theme.quoted,

                    _ => self.theme.normal,
                };

                res += &format!("\x01{}\x02{}\x01{}\x02",
                    style.prefix(),
                    &String::from_iter(&buf[pos..item.end]),
                    style.suffix(),
                );

                pos = item.end;
            }
        }

        if pos < buf.len() {
            res += &String::from_iter(&buf[pos..]);
        }

        if res.is_empty() {
            None
        } else {
            Some((res, start))
        }
    }
}

#[cfg(test)]
fn parse(buf: &str) -> Result<Vec<Item>, pom::Error> {
    let buf: Vec<char> = buf.chars().collect();
    PARSER.parse(Arc::new(InputV { input: buf.clone() }))
}


#[test]
fn test_syntax_parser() {
    assert_eq!(
        parse("AA=abcd"),
        Ok(vec![
            Item::new_with_children(Type::Assignment, vec![
                Item::new(0, 2, Type::Variable),
                Item::new(2, 3, Type::Symbol),
                Item::new(3, 7, Type::Initializer)
            ])
        ])
    );

    assert_eq!(
        parse("AA=\"hello world\""),
        Ok(vec![
            Item::new_with_children(Type::Assignment, vec![
                Item::new(0, 2, Type::Variable),
                Item::new(2, 3, Type::Symbol),
                Item::new_with_children(Type::Initializer, vec![
                    Item::new(3, 4, Type::Symbol),
                    Item::new(4, 15, Type::Quoted),
                    Item::new(15, 16, Type::Symbol),
                ])
            ])
        ])
    );

    assert_eq!(
        parse("ls -al && echo A"),
        Ok(vec![
            Item::new_with_children(Type::SimpleCommand, vec![
                Item::new(0, 2, Type::Command),
                Item::new(3, 6, Type::Argument),
            ]),

            Item::new(7, 9, Type::Symbol),

            Item::new_with_children(Type::SimpleCommand, vec![
                Item::new(10, 14, Type::Command),
                Item::new(15, 16, Type::Argument),
            ])
        ])
    );
}