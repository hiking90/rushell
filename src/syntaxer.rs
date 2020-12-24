use std::sync::Arc;
use linefeed::syntaxer;

use crate::theme;
use pom::parser::*;
use crate::parser::{space};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type {
    Command,
    Argument,
    Path,
    Keyword,
    Symbol,
    Variable,
    Quoted,
    Initializer,
    Redirection,
    RedirectionTarget,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Item {
    pub begin: usize,
    pub end: usize,
    pub type_of: Type,
}

impl Item {
    pub fn new(begin: usize, end: usize, type_of: Type) -> Self {
        Item {
            begin, end, type_of,
        }
    }
}


fn newline_list<'a>() -> Parser<'a, char, ()> {
    let comment = sym('#') * none_of("\n").repeat(0..);
    (space() * comment.opt() * sym('\n') * empty().pos()).repeat(1..)
    .discard()
    .name("newline_list")
}

fn linebreak<'a>() -> Parser<'a, char, ()> {
    newline_list() | space()
}

// fn separator_op<'a>() -> Parser<'a, char, bool> {
//     ((!tag(";;") * sym(';')).map(|_| false) | (!tag("&&") * sym('&')).map(|_| true)) - space()
// }

fn word_char<'a>() -> Parser<'a, char, char> {
    none_of("${}|&;()<>` \t\r\n\"")
}

fn tag_range<'a>(t: &'static str) -> Parser<'a, char, Item> {
    tag(t).range()
    .map(|r| Item::new(r.0, r.1, Type::Symbol))
}

fn expr_span<'a>() -> Parser<'a, char, Vec<Item>> {
    (
        tag_range("$((")
        - (!tag("))") * any()).repeat(1..)
        + tag_range("))")
    ).map(|(s1, s2)| vec![s1, s2])
}

fn backtick_span<'a>() -> Parser<'a, char, Vec<Item>> {
    (sym_range('`') - (!tag("\\`") * none_of("`")).repeat(0..) + sym_range('`').expect("`"))
    .map(|(s1, s2)| vec![s1, s2])
}

fn command_span<'a>() -> Parser<'a, char, Vec<Item>> {
    (tag_range("$(") - (!tag("\\)") * none_of(")")).repeat(0..) + sym_range(')'))
    .map(|(s1, s2)| vec![s1, s2])
}

fn expandable_var_name<'a>() -> Parser<'a, char, ()> {
    var_name()
    | (
        one_of("?$!*@#-")
        | is_a(|c: char| c.is_ascii_digit())
    ).discard()
}

fn var_name<'a>() -> Parser<'a, char, ()> {
    is_a(|c: char| c == '_' || c.is_ascii_alphanumeric()).repeat(1..)
    .discard()
}


fn param_span<'a>() -> Parser<'a, char, Vec<Item>> {
    sym_range('$').map(|s| vec![s]) - expandable_var_name()
}

fn param_ex_span<'a>() -> Parser<'a, char, Vec<Item>> {
    (
        tag_range("${")
        - (!tag("\\}") * none_of("}")).repeat(1..)
        + sym_range('}')
    ).map(|(s1, s2)| vec![s1, s2])
}

fn literal_in_double_quoted_span<'a>() -> Parser<'a, char, ()> {
    (
        (
            tag("\\\n").opt() *
            // The backslash retains its special meaning only when followed by one of the following characters:
            // ‘$’, ‘`’, ‘"’, ‘\’, or newline.
            (
                (sym('\\') * one_of("\\$`\"\""))
                | none_of("\"$`")
            )
        ).repeat(1..).discard()

        | (sym('$') + one_of(" \t\r").repeat(1..) + any()).collect().discard()
    )
    | (sym('$') - (-sym('\"'))).discard()
}


fn double_quoted_span<'a>() -> Parser<'a, char, Vec<Item>> {
    (
        sym_range('\"') +
        (
            expr_span()
            | backtick_span()
            | command_span()
            | param_span()
            | param_ex_span()
            | literal_in_double_quoted_span().range().map(|r| vec![Item::new(r.0, r.1, Type::Quoted)])
        ).repeat(0..)
        + sym_range('\"').expect("\"")
    ).map(|((s1, spans), s2)| {
        let mut res = vec![s1];
        res.append(&mut spans.into_iter().flatten().collect::<Vec<Item>>());
        res.push(s2);
        res
    })
}

fn single_quoted_span<'a>() -> Parser<'a, char, Vec<Item>> {
    (
        sym_range('\'') -
        (
            tag("\\\'").map(|_| '\'')
            | none_of("\'")
        ).repeat(1..)
        + sym_range('\'').expect("\'")
    ).map(|(s1, s2)| vec![s1, s2])
}

fn literal_span<'a>() -> Parser<'a, char, Vec<Item>> {
    (tag("\\\n").opt() * ((sym('\\') * any()) | word_char()))
    .repeat(1..).range()
    .map(|r| vec![Item::new(r.0, r.1, Type::Argument)])
}

fn word<'a>() -> Parser<'a, char, Vec<Item>> {
    !sym('#') *     // Add this to detect comment
    (
        double_quoted_span()
        | param_span()
        | param_ex_span()
        | expr_span()
        | command_span()
        | backtick_span()
        // | command_substitution_span()
        | single_quoted_span()
        // | proc_subst_span()
        // | brace_literal_span()
        | literal_span()
    ).repeat(1..)
    .map(|spans| spans.into_iter().flatten().collect::<Vec<Item>>())

    - space()
}

fn io_redirect<'a>() -> Parser<'a, char, Vec<Item>> {
    (
        (
            one_of("0123456789").opt() *
            (
                tag("<") |
                tag("<&") |
                tag(">") |
                tag(">|") |
                tag(">&") |
                tag(">>&") |
                tag(">>") |
                tag("<>")
            )
        ).range()
        .map(|r| Item::new(r.0, r.1, Type::Redirection))

        - space()

        + word().range()
        .map(|r| Item::new(r.0, r.1, Type::RedirectionTarget))
    ).map(|(i1, i2)| vec![i1, i2])
}


fn initializer<'a>() -> Parser<'a, char, Vec<Item>> {
    (sym_range('(') - space() - (word() - space()).repeat(0..).discard() + sym_range(')'))
        .map(|(s1, s2)| vec![s1, s2])
    | word()
}

fn assignment<'a>() -> Parser<'a, char, Vec<Item>> {
    (
        none_of(" \t\r\n+=").repeat(1..).range()
        .map(|r| Item::new(r.0, r.1, Type::Variable))

        + (tag("=") | tag("+=")).range()
        .map(|r| Item::new(r.0, r.1, Type::Symbol))

        + initializer().opt()
    ).map(|((var, symbol), init)| {
        let mut res = vec![];

        res.push(var);
        res.push(symbol);
        init.map(|mut items| res.append(&mut items));

        res
    })
     - space()
}

fn cmd_prefix<'a>() -> Parser<'a, char, Vec<Item>> {
    (
        assignment() |
        io_redirect()
    ).repeat(0..)
    .map(|v| v.into_iter().flatten().collect())
}


fn cmd_suffix<'a>() -> Parser<'a, char, Vec<Item>> {
    (io_redirect() | word()).repeat(0..)
    .map(|v| v.into_iter().flatten().collect())
}

fn simple_command<'a>() -> Parser<'a, char, Vec<Item>> {
    (
        cmd_prefix()
        + word().range().map(|r| Item::new(r.0, r.1, Type::Command))
        + cmd_suffix()
    ).map(|((mut prefix, command), mut suffix)| {
        prefix.push(command);
        prefix.append(&mut suffix);
        prefix
    })
}

fn assignment_command<'a>() -> Parser<'a, char, Vec<Item>> {
    assignment().repeat(1..)
    .map(|a| a.into_iter().flatten().collect())
}


fn command<'a>() -> Parser<'a, char, Vec<Item>> {
    space() *
    (
        // compound_command()
        // // | compound_command() + redirect_list()
        // | function_definition()
        // | local_definition()
        // | return_command()
        // | break_command()
        // | continue_command()
        // | simple_command()
        simple_command()
        | assignment_command()
    )
}

fn sym_range<'a>(symbol: char) -> Parser<'a, char, Item> {
    sym(symbol).range().map(|(begin, end)| Item { begin, end, type_of: Type::Symbol })
}

fn pipeline<'a>() -> Parser<'a, char, Vec<Item>> {
    let cmds = sym_range('|') - linebreak() + command();

    (command() + cmds.repeat(0..))
    .map(|(mut res, cmds)| {
        cmds.into_iter().for_each(|(symbol, mut cmd)| {
            res.push(symbol);
            res.append(&mut cmd);
        });
        res
    })
}


fn and_or<'a>() -> Parser<'a, char, Vec<Item>> {
    let others = (tag("||") | tag("&&")).range().map(|(begin, end)| Item { begin, end, type_of: Type::Symbol })
        - linebreak() + pipeline();

    space() *
    (pipeline() + others.repeat(0..))
    .map(|(mut res, others)| {
        others.into_iter().for_each(|(symbol, mut pipe)| {
            res.push(symbol);
            res.append(&mut pipe);
        });

        res
    })
}


fn list<'a>() -> Parser<'a, char, Vec<Item>> {
    let others = separator_op() + and_or();
    (and_or() + others.repeat(0..)).map(|(mut items, others)|
    {
        others.into_iter().for_each(|(bg, mut and_or)| {
            items.push(bg);
            items.append(&mut and_or);
        });
        items
    })
}

fn separator_op<'a>() -> Parser<'a, char, Item> {
    ((!tag(";;") * sym(';')).map(|_| false) | (!tag("&&") * sym('&')).map(|_| true)).range()
    .map(|(begin, end)| Item { begin, end, type_of: Type::Symbol })
    - space()
}


fn complete_command<'a>() -> Parser<'a, char, Vec<Item>> {
    (list() + separator_op().opt())
    .map(|(mut items, symbol)| {
        symbol.map(|symbol| items.push(symbol));
        items
    })
}

fn complete_commands<'a>() -> Parser<'a, char, Vec<Item>> {
    newline_list().map(|_| vec![]) |

    (
        linebreak() *
        complete_command() +
        (newline_list() * complete_command()).repeat(0..)
    ).map(|(mut cmd, cmds)| {
        cmd.append(&mut cmds.into_iter().flatten().collect::<Vec<Item>>());
        cmd
    })
}

pub struct Syntaxer {
    theme: theme::SyntaxTheme,
    parser: Parser<'static, char, Vec<Item>>,
}

impl Syntaxer {
    pub fn new() -> Syntaxer {
        Syntaxer {
            theme: theme::default_syntax_theme(),
            parser: complete_commands(),
        }
    }

    fn parse(&self, buf: &str) -> Result<Vec<Item>, pom::Error> {
        let input: Vec<char> = buf.chars().collect();
        self.parser.parse(Arc::new(InputV { input: input }))
    }
}

impl syntaxer::Syntaxer for Syntaxer {
    fn highlight(&self, buf: &str, pos: usize) -> Option<String> {
        let items = self.parse(buf).ok()?;

        // println!("{:?}", items);

        let items = items.into_iter().filter(|item| pos <= item.end).collect::<Vec<Item>>();

        let mut res = String::new();
        let mut pos = pos;

        for item in items {
            if item.begin > pos {
                res += &buf[pos .. item.begin];
                pos = item.begin;
            }

            let style = match item.type_of {
                Type::Command => self.theme.command,
                Type::Argument => self.theme.argument,
                Type::Path => self.theme.valid_path,
                Type::Quoted |
                Type::Keyword |
                Type::Symbol |
                Type::Variable |
                Type::Initializer |
                Type::Redirection |
                Type::RedirectionTarget => self.theme.quoted,
            };

            res += &format!("\x01{}\x02{}\x01{}\x02",
                style.prefix(),
                &buf[pos..item.end],
                style.suffix(),
            );

            pos = item.end;
        }

        if pos < buf.len() {
            res += &buf[pos..];
        }

        // let mut copied = pos;

        // for word in input.words() {
        //     let mut start = word.start;

        //     if start > pos || (start <= pos && pos < word.end) {
        //         if start <= pos {
        //             start = pos;
        //         }
        //         if start > copied {
        //             res += &buf[copied .. start];
        //         }

        //         let style = if let input::Quoted::True(_) = word.quoted {
        //             self.theme.quoted
        //         } else {
        //             match word.kind {
        //                 input::Kind::Command => self.theme.command,
        //                 input::Kind::Argument => self.theme.argument,
        //                 input::Kind::ValidPath => self.theme.valid_path,
        //                 _ => self.theme.normal,
        //             }
        //         };

        //         res += &format!("\x01{}\x02{}\x01{}\x02",
        //             style.prefix(),
        //             &buf[start..word.end],
        //             style.suffix(),
        //         );

        //         copied = word.end;
        //     }
        // }

        // if copied < buf.len() {
        //     res += &buf[copied..];
        // }

        if res.is_empty() {
            None
        } else {
            Some(res)
        }
    }
}

#[test]
fn test_syntax_parser() {
    let parser = Syntaxer::new();

    assert_eq!(
        parser.parse("AA=abcd"),
        Ok(vec![
            Item::new(0, 2, Type::Variable),
            Item::new(2, 3, Type::Symbol),
            Item::new(3, 7, Type::Argument),
        ])
    );

    assert_eq!(
        parser.parse("AA=\"hello world\""),
        Ok(vec![
            Item::new(0, 2, Type::Variable),
            Item::new(2, 3, Type::Symbol),
            Item::new(3, 4, Type::Symbol),
            Item::new(4, 15, Type::Quoted),
            Item::new(15, 16, Type::Symbol),
        ])
    );
}