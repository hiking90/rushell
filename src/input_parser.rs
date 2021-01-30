use pom::parser::*;
use crate::parser::{space};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type {
    SimpleCommand,
    Command,
    Argument,
    Assignment,
    Redirect,
    Literal,
    // Path,
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
    pub children: Vec<Item>,
}

impl Item {
    pub fn new(begin: usize, end: usize, type_of: Type) -> Self {
        Item {
            begin, end, type_of, children: vec![],
        }
    }

    pub fn new_with_children(type_of: Type, children: Vec<Item>) -> Self {
        Item {
            begin: children.first().unwrap().begin,
            end: children.last().unwrap().end,
            type_of: type_of,
            children: children,
        }
    }
}


fn newline_list<'a>() -> Parser<'a, char, ()> {
    let comment = sym('#') * none_of("\n").repeat(0..);
    (space() * comment.opt() * sym('\n')).repeat(1..)
    .discard()
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
    // (sym_range('`') - (!tag("\\`") * none_of("`")).repeat(0..) + sym_range('`').expect("`"))
    (sym_range('`') + compound_list() + sym_range('`'))
    .map(|((s1, mut items), s2)| {
        let mut res = vec![s1];
        res.append(&mut items);
        res.push(s2);

        res
    })
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

        | (sym('$') + one_of(" \t\r").repeat(1..)).collect().discard()
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
    .map(|r| vec![Item::new(r.0, r.1, Type::Literal)])
}

fn word<'a>(type_of: Type) -> Parser<'a, char, Item> {
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
    .map(move |spans| {
        let children = spans.into_iter().flatten().collect::<Vec<Item>>();
        // If there is only one child, the child should be ignored to make simple Item structure.
        if children.len() > 1 {
            Item::new_with_children(type_of.clone(), children)
        } else {
            let item = children.first().unwrap();
            Item::new(item.begin, item.end, type_of.clone())
        }
    })

    - space()
}

fn io_redirect<'a>() -> Parser<'a, char, Item> {
    (
        (
            one_of("0123456789").opt() *
            (
                tag("<") |
                tag("<&") |
                tag(">") |
                tag(">|") |
                tag(">&") |
                tag("&>") |
                tag(">>&") |
                tag(">>") |
                tag("<>")
            )
        ).range()
        .map(|r| Item::new(r.0, r.1, Type::Redirection))

        - space()

        + word(Type::RedirectionTarget)
    ).map(|(i1, i2)| {
        Item::new_with_children(Type::Redirect, vec![i1, i2])
    })
}


fn initializer<'a>() -> Parser<'a, char, Item> {
    (sym_range('(') - space() - word(Type::Initializer).repeat(0..).discard() + sym_range(')'))
        .map(|(s1, s2)| Item::new_with_children(Type::Initializer, vec![s1, s2]))
    | word(Type::Initializer)
}

fn assignment<'a>() -> Parser<'a, char, Item> {
    (
        none_of(" \t\r\n+=").repeat(1..).range()
        .map(|r| Item::new(r.0, r.1, Type::Variable))

        + (tag("=") | tag("+=")).range()
        .map(|r| Item::new(r.0, r.1, Type::Symbol))

        + initializer().opt()
    ).map(|((var, symbol), init)| {
        let mut v = vec![];

        v.push(var);
        v.push(symbol);
        init.map(|init| v.push(init));

        Item::new_with_children(Type::Assignment, v)
    })
     - space()
}

fn cmd_prefix<'a>() -> Parser<'a, char, Vec<Item>> {
    (
        assignment() |
        io_redirect()
    ).repeat(0..)
}

fn cmd_suffix<'a>() -> Parser<'a, char, Vec<Item>> {
    (io_redirect() | word(Type::Argument)).repeat(0..)
}

fn simple_command<'a>() -> Parser<'a, char, Vec<Item>> {
    (
        cmd_prefix()
        + word(Type::Command)
        + cmd_suffix()
    ).map(|((mut prefix, command), mut suffix)| {
        let mut simple_command = vec![command];
        simple_command.append(&mut suffix);

        prefix.push(Item::new_with_children(Type::SimpleCommand, simple_command));
        prefix
    })
}

fn assignment_command<'a>() -> Parser<'a, char, Vec<Item>> {
    assignment().repeat(1..)
    // .map(|a| a.into_iter().flatten().collect())
}

fn keyword_command<'a>() -> Parser<'a, char, Vec<Item>> {
    (tag("break") | tag("continue")).range()
    .map(|r| vec![Item::new(r.0, r.1, Type::Command)])
}

fn num<'a>() -> Parser<'a, char, Vec<Item>> {
    is_a(|c: char| c.is_ascii_digit()).repeat(1..).range()
    .map(|r| vec![Item::new(r.0, r.1, Type::Argument)])
}

fn return_command<'a>() -> Parser<'a, char, Vec<Item>> {
    (
        tag("return").range()
        .map(|r| Item::new(r.0, r.1, Type::Command))

        - space()
        + (
            num()
            | param_span()
        ).opt()
    ).map(|(r1, r2)| {
        let mut res = vec![r1];
        r2.map(|mut r| res.append(&mut r));

        res
    })
}

fn local_definition<'a>() -> Parser<'a, char, Vec<Item>> {
    (
        tag("local").range()
        .map(|r| Item::new(r.0, r.1, Type::Command))

        - space() +
        (
            assignment()
            | var_name().range().map(|r| Item::new(r.0, r.1, Type::Argument))
            - space()
        ).repeat(1..)
    ).map(|(i, mut items)| {
        let mut res = vec![i];
        res.append(&mut items);
        res
    })
}

fn command<'a>() -> Parser<'a, char, Vec<Item>> {
    space() *
    (
        // compound_command()
        // // | compound_command() + redirect_list()
        // | function_definition()
        local_definition()
        | return_command()
        | keyword_command()
        | simple_command()
        | assignment_command()
    )
}

fn sym_range<'a>(symbol: char) -> Parser<'a, char, Item> {
    sym(symbol).range().map(|r| Item::new(r.0, r.1, Type::Symbol))
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
    let others = (tag("||") | tag("&&")).range().map(|r| Item::new(r.0, r.1, Type::Symbol))
        - linebreak() + pipeline().opt();

    space() *
    (pipeline() + others.repeat(0..))
    .map(|(mut res, others)| {
        others.into_iter().for_each(|(symbol, pipe)| {
            res.push(symbol);
            pipe.map(|mut pipe| res.append(&mut pipe));
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

fn compound_list<'a>() -> Parser<'a, char, Vec<Item>> {
    call(_compound_list)
}

fn _compound_list<'a>() -> Parser<'a, char, Vec<Item>> {
    (linebreak() * term() + separator().opt())
    .map(|(mut items, bg)| {
        bg.map(|bg| items.push(bg));

        items
    })
}

fn term<'a>() -> Parser<'a, char, Vec<Item>> {
    let others = separator() + and_or();
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
    .map(|r| Item::new(r.0, r.1, Type::Symbol))
    - space()
}

fn separator<'a>() -> Parser<'a, char, Item> {
    (separator_op() - linebreak())
    | newline_list().range().map(|r| Item::new(r.0, r.1, Type::Symbol))
}


fn complete_command<'a>() -> Parser<'a, char, Vec<Item>> {
    (list() + separator_op().opt())
    .map(|(mut items, symbol)| {
        symbol.map(|symbol| items.push(symbol));
        items
    })
}

pub fn complete_commands<'a>() -> Parser<'a, char, Vec<Item>> {
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

lazy_static! {
    pub static ref PARSER: Parser<'static, char, Vec<Item>> = complete_commands();
}
