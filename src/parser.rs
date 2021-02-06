use pom::parser::*;

use std::sync::Arc;
use std::iter::FromIterator;
use std::os::unix::io::RawFd;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum RedirectionDirection {
    Input,  // cat < foo.txt or here document
    Output, // cat > foo.txt
    Append, // cat >> foo.txt
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum RedirectionType {
    File(Word),
    Fd(RawFd),
    FileOrFd(Word),
    HereDoc(HereDoc),
    /// Since the contents of a here document comes after the current
    /// elemenet (e.g., Command::SimpleCommand), the parser memorizes the
    /// index of here document in `UnresolvedHereDoc` and replace this
    /// with `HereDoc` later. Used internally by the parser.
    UnresolvedHereDoc {
        delimiter: String,
        quoted: bool,
        trim: bool,
    },  // DELIMITER
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Redirection {
    pub fd: usize,
    pub direction: RedirectionDirection,
    pub target: RedirectionType,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum RunIf {
    Always,
    /// Run the command if the previous command returned 0.
    Success,
    /// Run the command if the previous command returned non-zero value.
    Failure,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct CaseItem {
    pub patterns: Vec<Word>,
    pub body: Vec<Term>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ElIf {
    pub condition: Vec<Term>,
    pub then_part: Vec<Term>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Initializer {
    Array(Vec<Word>),
    String(Word),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Assignment {
    pub name: String,
    pub initializer: Initializer,
    pub index: Option<Box::<Expr>>,
    pub append: bool,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum LocalDeclaration {
    // local foo=123
    // local bar[0]=123 (same as `local bar=(123)`)
    Assignment(Assignment),
    // local foo
    Name(String),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Command {
    SimpleCommand {
        external: bool,
        argv: Vec<Word>,
        redirects: Vec<Redirection>,
        /// Assignment prefixes. (e.g. "RAILS_ENV=production rails server")
        assignments: Vec<Assignment>,
    },
    // foo=1, bar="Hello World", ...
    Assignment {
        assignments: Vec<Assignment>,
    },
    If {
        condition: Vec<Term>,
        then_part: Vec<Term>,
        elif_parts: Vec<ElIf>,
        else_part: Option<Vec<Term>>,
        redirects: Vec<Redirection>,
    },
    While {
        condition: Vec<Term>,
        body: Vec<Term>,
    },
    For {
        var_name: String,
        words: Vec<Word>,
        body: Vec<Term>,
    },
    ArithFor {
        init: Box<Expr>,
        cond: Box<Expr>,
        update: Box<Expr>,
        body: Vec<Term>,
    },
    Break,
    Continue,
    Return {
        status: Option<Word>,
    },
    Case {
        word: Word,
        cases: Vec<CaseItem>,
    },
    FunctionDef {
        name: String,
        body: Box<Command>, // Typically Command::Group.
    },
    LocalDef {
        declarations: Vec<LocalDeclaration>,
    },
    Group {
        terms: Vec<Term>,
    },
    SubShellGroup {
        terms: Vec<Term>,
    },
    Cond{is_not: bool, expr: Option<Box<CondExpr>>},
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum CondExpr {
    Or(Box<CondExpr>, Box<CondExpr>),
    And(Box<CondExpr>, Box<CondExpr>),
    // = or ==
    StrEq(Box<CondExpr>, Box<CondExpr>),
    // !=
    StrNe(Box<CondExpr>, Box<CondExpr>),
    // -eq
    Eq(Box<CondExpr>, Box<CondExpr>),
    Ne(Box<CondExpr>, Box<CondExpr>),
    Lt(Box<CondExpr>, Box<CondExpr>),
    Le(Box<CondExpr>, Box<CondExpr>),
    Gt(Box<CondExpr>, Box<CondExpr>),
    Ge(Box<CondExpr>, Box<CondExpr>),
    Regex(Box<CondExpr>, String),
    File(Box<CondExpr>, String),
    Word(Word),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Pipeline {
    pub run_if: RunIf,
    pub commands: Vec<Command>, // Separated by `|'.
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Term {
    // pub span: (usize, usize),
    pub code: String,
    pub pipelines: Vec<Pipeline>, // Separated by `&&' or `||'.
    pub background: bool,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Ast {
    pub terms: Vec<Term>, // Separated by `&', or `;'.
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ParseError {
    Fatal(String),
    Empty,
    Expected(String),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ExpansionOp {
    Prefix(char),                        // ${!prefix*} or ${!prefix@}
    Indices(char),                       // ${!name[*]} or ${!name[@]}
    Length,                              // ${#parameter}
    GetOrEmpty,                          // $parameter and ${parameter}
    GetOrAction(String, Word),           // ${parameter-word}
    // ${parameter/pattern/replacement}
    Subst {
        pattern: String,
        replacement: String,
        op: Option<char>,                       // one of "/#%"
    },
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct BinaryExpr {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expr {
    Add(BinaryExpr),
    Sub(BinaryExpr),
    Mul(BinaryExpr),
    Div(BinaryExpr),
    Modulo(BinaryExpr),
    Assign { name: String, rhs: Box<Expr> },
    Literal(i32),

    Minus(Box<Expr>),

    // `foo` in $((foo + 1))
    Parameter { name: String },

    // Conditions. Evaluated to 1 if it satistifies or 0 if not.
    Eq(Box<Expr>, Box<Expr>),
    Ne(Box<Expr>, Box<Expr>),
    Lt(Box<Expr>, Box<Expr>),
    Le(Box<Expr>, Box<Expr>),
    Gt(Box<Expr>, Box<Expr>),
    Ge(Box<Expr>, Box<Expr>),

    // `i++` and `i--`
    Inc(String),
    Dec(String),

    Word(Word),

    // Expr(Box<Expr>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ProcSubstType {
    // <(echo hello)
    StdoutToFile,
    // >(grep hello)
    FileToStdin,
}

// #[derive(Debug, PartialEq, Eq, Clone)]
// pub enum LiteralChar {
//     Normal(char),
//     Escaped(char),
// }

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Span {
    Literal(String),
    // LiteralChars(Vec<LiteralChar>),
    // ~, ~mike, ...
    Tilde(Option<String>),
    // $foo, ${foo}, ${foo:-default}, ...
    Parameter {
        name: Box<Span>,
        index: Option<Box<Span>>,
        op: ExpansionOp,
        quoted: bool,
    },
    // $${foo[1]} ...
    // ArrayParameter {
    //     name: String,
    //     index: Box<Span>,
    //     quoted: bool,
    // },
    // $(echo hello && echo world)
    Command {
        body: Vec<Term>,
        quoted: bool,
    },
    // <(echo hello)
    ProcSubst {
        body: Vec<Term>,
        subst_type: ProcSubstType,
    },
    // $((1 + 2 * 3))
    ArithExpr {
        expr: Box<Expr>,
    },

    SubString {
        param: Box<Span>,
        offset: Box<Expr>,
        length: Option<Box<Expr>>,
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Word(pub Vec<Span>);

impl Word {
    #[inline]
    pub fn spans(&self) -> &[Span] {
        &self.0
    }
}

/// Contains heredoc body. The outer Vec represents lines and
/// `Vec<Word>` represents the contents of a line.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct HereDoc(Vec<Word>);

impl HereDoc {
    pub fn lines(&self) -> &Vec<Word> {
        &self.0
    }
}


/////////////////////////////////////

const WHITESPACE: &str = " \t\r";

pub fn space<'a>() -> Parser<'a, char, ()> {
    (silent_char() | one_of(WHITESPACE).discard()).repeat(0..).discard()
}

fn newline_list<'a>() -> Parser<'a, char, ()> {
    let comment = sym('#') * none_of("\n").repeat(0..);
    (space() * comment.opt() * sym('\n')).repeat(1..)
    .discard()
    .name("newline_list")
}

fn linebreak<'a>() -> Parser<'a, char, ()> {
    newline_list() | space()
}

fn separator_op<'a>() -> Parser<'a, char, bool> {
    space() * ((!tag(";;") * sym(';')).map(|_| false) | (!tag("&&") * sym('&')).map(|_| true)) - space()
}

fn separator<'a>() -> Parser<'a, char, bool> {
    (separator_op() - linebreak().opt())
    | newline_list().map(|_| false)
}

pub fn reserved_word<'a>() -> Parser<'a, char, ()> {
    (
    tag("case")
    | tag("continue")
    | tag("break")
    | tag("done")
    | tag("do")
    | tag("elif")
    | tag("else")
    | tag("esac")
    | tag("fi")
    | tag("for")
    | tag("function")
    | tag("if")
    | tag("in")
    // | tag("local")
    | tag("return")
    | tag("then")
    | tag("while")
    | tag("[[")
    ) * (!word_char("")).discard()
}

enum WordType {
    CmdName,
    CmdWord,
}

fn word_char<'a>(dynot: &'static str) -> Parser<'a, char, char> {
    !one_of(dynot) * none_of("${}|&;()<>` \t\r\n\"")
    // !one_of("|&; \t\r\n\"") * !one_of(dynot) * any()
}

pub fn silent_char<'a>() -> Parser<'a, char, ()> {
    tag("\\\n").discard()
}

fn escape_sequence<'a>() -> Parser<'a, char, char> {
    sym('\\') * any()
}

fn literal_in_double_quoted_span<'a>() -> Parser<'a, char, Span> {
    (
        (
            silent_char().opt() *
            // The backslash retains its special meaning only when followed by one of the following characters:
            // ‘$’, ‘`’, ‘"’, ‘\’, or newline.
            (
                (sym('\\') * one_of("\\$`\""))
                | none_of("\"$`\n")
            )
        ).repeat(1..)

        // | (sym('$') + one_of(WHITESPACE).repeat(1..)).collect()
        | (silent_char().opt() * sym('$') + none_of("0123456789*@#?-$!({\"")).collect()
        .map(|a| Vec::from(a))
    ).map(|chars| {
        Span::Literal(String::from_iter(chars.iter()))
    })
    .name("literal_in_double_quoted_span")

    | (sym('$') - (-sym('\"'))).map(|ch| Span::Literal(ch.to_string()))
}

fn double_quoted_span<'a>() -> Parser<'a, char, Vec<Span>> {
    (
        sym('\"') *
        (
            expr_span()
            | backtick_span()
            | command_span()
            | param_span()
            | param_ex_span()
            | literal_in_double_quoted_span()
        ).repeat(0..)
        - sym('\"').expect("\"")
    ).map(|mut spans| {
        spans.iter_mut().for_each(|span| {
            match span {
                Span::Parameter{name: _, op: _, quoted, ..} => *quoted = true,
                // Span::ArrayParameter{name: _, index: _, quoted} => *quoted = true,
                Span::Command{body: _, quoted} => *quoted = true,
                _ => {}
            }
        });
        spans
    })
}

// single_quoted_span = { "'" ~ single_quoted_span_inner* ~ "'" }
// single_quoted_span_inner = _{
//     literal_in_single_quoted_span
// }
// literal_in_single_quoted_span = ${ ( "\\'"  | !("\'") ~ ANY | WHITESPACE)+ }
fn single_quoted_span<'a>() -> Parser<'a, char, Span> {
    (
        sym('\'') *
        (
            tag("\\\'").map(|_| '\'')
            | none_of("\'")
        ).repeat(1..)
        - sym('\'').expect("\'")
    ).map(|chars| {
        Span::Literal(String::from_iter(chars.iter()))
    })
    .name("single_quoted_span")
}

// command_span = !{ "$(" ~ compound_list ~ ")" }
fn command_span<'a>() -> Parser<'a, char, Span> {
    (tag("$(") * compound_list() - sym(')').expect(")"))
    .map(|cmd| {
        Span::Command {
            body: cmd,
            quoted: false,
        }
    })
}

fn literal_span<'a>(dynot: &'static str) -> Parser<'a, char, Vec<Span>> {
    (silent_char().opt() * (escape_sequence() | word_char(dynot))).repeat(1..)
    .map(|chars| {

        // Tilde parser is implemented from scratch because of performance concern.
        let mut spans = vec![];
        let mut literal = String::new();

        let mut it = chars.iter().enumerate().peekable();

        loop {
            match it.next() {
                Some((i, ch)) if *ch == '=' || *ch == ':' || (i == 0 && *ch == '~') => {
                    if *ch != '~' {
                        literal.push(*ch);

                        match it.peek() {
                            Some((_, '~')) => it.next(),
                            _ => continue,
                        };
                    }

                    if literal.len() > 0 {
                        spans.push(Span::Literal(literal));
                        literal = String::new();
                    }

                    let mut username = String::new();
                    loop {
                        match it.peek() {
                            Some((_, ch)) if !" \t/:=".contains(**ch) => {
                                username.push(**ch);
                                it.next();
                            }
                            _ => break,
                        }
                    }

                    spans.push(Span::Tilde(if username.len() > 0 { Some(username) } else { None }));
                }
                None => break,
                Some((_, ch)) => literal.push(*ch),
            }
        }

        if literal.len() > 0 {
            spans.push(Span::Literal(literal));
        }

        spans
    })
    .name("literal_span")
}

// param_span = { "$" ~ expandable_var_name }
fn param_span<'a>() -> Parser<'a, char, Span> {
    (sym('$') * expandable_var_name())
    .map(|name| {
        Span::Parameter {
            name: Box::new(Span::Literal(name)),
            index: None,
            op: ExpansionOp::GetOrEmpty,
            quoted: false,
        }
    })
}

// expr_span = !{ "$((" ~ expr ~ "))" }
fn expr_span<'a>() -> Parser<'a, char, Span> {
    (tag("$((") * space() * expr() - space() - tag("))").expect("))"))
    .map(|expr| Span::ArithExpr{expr})
}

// backtick_span = !{ "`" ~ compound_list ~ "`" }
fn backtick_span<'a>() -> Parser<'a, char, Span> {
    (sym('`') * none_of("`").repeat(0..) - sym('`').expect("`"))
    .custom_parser(|chars, _, end| {
        let cmd = compound_list().parse(Arc::new(InputV {
            input: chars
        }))?;

        Ok((Span::Command {
            body: cmd,
            quoted: false,
        }, end))
    })
}

// proc_subst_direction = { "<(" | ">(" }
// proc_subst_span = !{ proc_subst_direction ~ compound_list ~ ")" }
fn proc_subst_span<'a>() -> Parser<'a, char, Span> {
    ((sym('<') | sym('>')) + sym('(') * compound_list() - sym(')').expect(")"))
    .map(|(dir, cmd)| {
        Span::ProcSubst {
            body: cmd,
            subst_type: if dir == '<' {
                ProcSubstType::StdoutToFile
            } else {
                ProcSubstType::FileToStdin
            },
        }
    })
}

// brace_literal_span = { "{" ~ literal_span ~ "}" }
fn brace_literal_span<'a>() -> Parser<'a, char, Span> {
    (sym('{') + literal_span("") + sym('}').expect("}")).collect()
    .map(|chars| {
        Span::Literal(String::from_iter(chars))
    })
}

fn word<'a>(word_type: WordType) -> Parser<'a, char, Word> {
    let not_set = match word_type {
        WordType::CmdName => "=",
        WordType::CmdWord => "",
    };

    !sym('#') *     // Add this to detect comment
    (
        double_quoted_span() |
        (
            param_span()
            | param_ex_span()
            | expr_span()
            | command_span()
            | backtick_span()
            | command_substitution_span()
            | single_quoted_span()
            | proc_subst_span()
            | brace_literal_span()
        ).map(|span| vec![span])
        | literal_span(not_set)
    ).repeat(1..)
    .map(|spans| {
        Word(spans.into_iter().flatten().collect())
    })
    .name("word")

    - space()
}

fn io_number<'a>() -> Parser<'a, char, u32> {
    one_of("0123456789").map(|ch| ch.to_digit(10).unwrap() - '0'.to_digit(10).unwrap())
}

fn filename<'a>() -> Parser<'a, char, Word> {
    word(WordType::CmdWord)
}

fn io_file_op<'a>(op: &'static str) -> Parser<'a, char, Word> {
    tag(op) * space() * filename()
}

fn io_file<'a>() -> Parser<'a, char, Vec<Redirection>> {
    io_file_op("<").map(|name| {
        vec![Redirection {
            fd: 0,
            direction: RedirectionDirection::Input,
            target: RedirectionType::File(name),
        }]
    }) |

    io_file_op("<&").map(|name| {
        vec![Redirection {
            fd: 0,
            direction: RedirectionDirection::Input,
            target: RedirectionType::FileOrFd(name),
        }]
    }) |

    (io_file_op(">") | io_file_op(">|")).map(|name| {
        vec![Redirection {
            fd: 1,
            direction: RedirectionDirection::Output,
            target: RedirectionType::File(name),
        }]
    }) |

    io_file_op(">&").map(|name| {
        vec![Redirection {
            fd: 1,
            direction: RedirectionDirection::Output,
            target: RedirectionType::FileOrFd(name),
        }]
    }) |

    io_file_op("&>").map(|name| {
        vec![Redirection {
            fd: 1,
            direction: RedirectionDirection::Output,
            target: RedirectionType::FileOrFd(name),
        },
        Redirection {
            fd: 2,
            direction: RedirectionDirection::Output,
            target: RedirectionType::Fd(1),
        }]
    }) |

    io_file_op(">>").map(|name| {
        vec![Redirection {
            fd: 1,
            direction: RedirectionDirection::Append,
            target: RedirectionType::File(name),
        }]
    }) |

    io_file_op("<>").map(|name| {
        vec![
            Redirection {
                fd: 0,
                direction: RedirectionDirection::Input,
                target: RedirectionType::File(name.clone()),
            },
            Redirection {
                fd: 1,
                direction: RedirectionDirection::Output,
                target: RedirectionType::File(name),
            }
        ]
    })
}

fn here_delimiter<'a>() -> Parser<'a, char, (String, bool)> {
    (sym('\"') * none_of("\"").repeat(1..) - sym('\"'))
    .map(|chars| (String::from_iter(chars), true))

    | (none_of(" \t\r\n").repeat(1..))
    .map(|chars| (String::from_iter(chars), false))
}

fn io_here<'a>() -> Parser<'a, char, Redirection> {
    (tag("<<") * sym('-').opt() - space() + here_delimiter().expect("here delimiter") - space())
    .map(|(trim, (delimiter, quoted))| {
        Redirection {
            fd: 0,
            direction: RedirectionDirection::Input,
            target: RedirectionType::UnresolvedHereDoc {
                delimiter: delimiter,
                quoted: quoted,
                trim: trim.is_some(),
            },
        }
    })
}

// io_redirect      :           io_file
//                  | IO_NUMBER io_file
//                  |           io_here
//                  | IO_NUMBER io_here
//                  ;
fn io_redirect<'a>() -> Parser<'a, char, Vec<Redirection>> {
    (io_number().opt() + io_file()).map(|(fd, mut redirects)| {
        if let Some(fd) = fd {
            redirects.first_mut().unwrap().fd = fd as usize;
        }
        redirects
    }) |

    (io_number().opt() + io_here()).map(|(fd, mut redirect)| {
        if let Some(fd) = fd {
            redirect.fd = fd as usize;
        }
        vec![redirect]
    }) |

    io_file_op(">&").map(|name| {
        vec![
            Redirection {
                fd: 1,
                direction: RedirectionDirection::Output,
                target: RedirectionType::File(name),
            },
            Redirection {
                fd: 2,
                direction: RedirectionDirection::Output,
                target: RedirectionType::Fd(1),
            }
        ]
    }) |

    io_file_op(">>&").map(|name| {
        vec![
            Redirection {
                fd: 1,
                direction: RedirectionDirection::Append,
                target: RedirectionType::File(name),
            },
            Redirection {
                fd: 2,
                direction: RedirectionDirection::Output,
                target: RedirectionType::Fd(1),
            }
        ]
    })
}

// cmd_suffix       :            io_redirect
//                  | cmd_suffix io_redirect
//                  |            WORD
//                  | cmd_suffix WORD
//                  ;
fn cmd_suffix<'a>() -> Parser<'a, char, (Vec<Word>, Vec<Redirection>)> {
    (io_redirect().map(|r| (None, Some(r))) | cmd_word().map(|w| (Some(w), None))).repeat(0..)
    .map(|v| {
        let mut res = (vec![], vec![]);

        for (w, r) in v {
            if let Some(w) = w {
                res.0.push(w)
            }
            if let Some(mut r) = r {
                res.1.append(&mut r)
            }
        }

        res
    })
}

//
//  Expr
//

fn expr<'a>() -> Parser<'a, char, Box<Expr>> {
    Parser::new(move |input: Arc<dyn Input<char>>, start: usize| {
        (EXPR.method)(input.clone(), start)
    })
}

// expr = !{ assign ~ (comp_op ~ expr)? }
fn _expr<'a>() -> Parser<'a, char, Box<Expr>> {
    (assign() - space() + (comp_op() - space() + expr()).opt())
    .map(|(lhs, rhs)|{
        if let Some((op, rhs)) = rhs {
            let expr = match op {
                "==" => Expr::Eq(lhs, rhs),
                "!=" => Expr::Ne(lhs, rhs),
                ">=" => Expr::Ge(lhs, rhs),
                ">" => Expr::Gt(lhs, rhs),
                "<=" => Expr::Le(lhs, rhs),
                "<" => Expr::Lt(lhs, rhs),
                _ => unreachable!(),
            };
            Box::new(expr)
        } else {
            lhs
        }
    })
}

// comp_op = { "==" | "!=" | ">=" | ">" | "<=" | "<" }
fn comp_op<'a>() -> Parser<'a, char, &'static str> {
    tag("==")
    | tag("!=")
    | tag(">=")
    | tag(">")
    | tag("<=")
    | tag("<")
}

// assign =
//        { (var_name ~ assign_op ~ assign)
//        | arith
//        }
fn assign<'a>() -> Parser<'a, char, Box<Expr>> {

    (var_name() - space() - sym('=') - space() + call(assign))
    .map(|(name, expr)|{
        Box::new(Expr::Assign{
            name: name,
            rhs: expr,
        })
    })

    | arith()
}

// arith = { term ~ (arith_op ~ arith)? }
fn arith<'a>() -> Parser<'a, char, Box<Expr>> {
    (arith_term() + (one_of("+-") - space() + call(arith) - space()).opt())
    .map(|(lhs, rhs)| {
        if let Some((op, rhs)) = rhs {
            match op {
                '+' => Box::new(Expr::Add(BinaryExpr{lhs: lhs, rhs: rhs})),
                '-' => Box::new(Expr::Sub(BinaryExpr{lhs: lhs, rhs: rhs})),
                _ => unreachable!(),
            }
        } else {
            lhs
        }
    })
}

// term = { factor ~ (factor_op ~ term)? }
fn arith_term<'a>() -> Parser<'a, char, Box<Expr>> {
    (factor() - space() + (one_of("*/%") - space() + call(arith_term) - space()).opt())
    .map(|(lhs, rhs)|{
        if let Some((op, rhs)) = rhs {
            match op {
                '*' => Box::new(Expr::Mul(BinaryExpr{lhs: lhs, rhs: rhs})),
                '/' => Box::new(Expr::Div(BinaryExpr{lhs: lhs, rhs: rhs})),
                '%' => Box::new(Expr::Modulo(BinaryExpr{lhs: lhs, rhs: rhs})),
                _ => unreachable!(),
            }
        } else {
            lhs
        }
    })
}

// factor = { sign ~ primary ~ postfix_incdec }
fn factor<'a>() -> Parser<'a, char, Box<Expr>> {
    (one_of("+-") - space() + call(factor))
    .map(|(sign, factor)| {
        if sign == '-' {
            Box::new(Expr::Minus(factor))
        } else {
            factor
        }
    })

    | (var_name() + (tag("++") | tag("--")))
    .map(|(name, incdec)| {
        if incdec == "++" {
            Box::new(Expr::Inc(name))
        } else {
            Box::new(Expr::Dec(name))
        }
    })

    | (primary() - space())
}

fn num<'a>() -> Parser<'a, char, String> {
    is_a(|c: char| c.is_ascii_digit()).repeat(1..)
    .map(|chars| {
        String::from_iter(chars.into_iter())
    })
    .name("num")
}

// primary = _{ num | ("$"? ~ var_name) | param_ex_span |  ("(" ~ expr ~ ")") }
fn primary<'a>() -> Parser<'a, char, Box<Expr>> {
    num().map(|n| Box::new(Expr::Literal(n.parse().unwrap())))     // TODO: Remove unwrap()
    | (sym('$').opt() * var_name()).map(|name| Box::new(Expr::Parameter { name }))
    | param_ex_span().map(|span| Box::new(Expr::Word(Word(vec![span]))))
    | (sym('(') * expr() - sym(')'))
}

fn param_ex_span<'a>() -> Parser<'a, char, Span> {
    Parser::new(move |input: Arc<dyn Input<char>>, start: usize| {
        (PARAM_EX_SPAN.method)(input.clone(), start)
    })
}

// param_ex_span = { "$" ~ "{" ~ length_op ~ expandable_var_name ~ index ~ param_opt? ~ "}" }
fn _param_ex_span<'a>() -> Parser<'a, char, Span> {
    tag("${")
    * (
        (
            parameter() +
            (
                tag(":-")
                | tag("-")
                | tag(":=")
                | tag("=")
                | tag(":?")
                | tag("?")
                | tag(":+")
                | tag("+")
            )
            + word(WordType::CmdWord)
        )
        .map(|(((name, idx), op), word)|{
            Span::Parameter {
                name: name,
                index: idx,
                op: ExpansionOp::GetOrAction(op.into(), word),
                quoted: false,
            }
        })

        | (parameter() - sym(':') + expr() + (sym(':') * expr()).opt())
        .map(|((param, offset), len)|{
            Span::SubString {
                param: Box::new(
                    Span::Parameter {
                        name: param.0,
                        index: param.1,
                        op: ExpansionOp::GetOrEmpty,
                        quoted: false,
                    }
                ),
                offset: offset,
                length: len,
            }
        })

        | (sym('!') * var_name() + one_of("*@"))
        .map(|(name, op)|{
            Span::Parameter{
                name: Box::new(Span::Literal(name)),
                index: None,
                op: ExpansionOp::Prefix(op),
                quoted: false,
            }
        })

        | (sym('!') * var_name() - sym('[') + one_of("*@") - sym(']'))
        .map(|(name, op)|{
            Span::Parameter{
                name: Box::new(Span::Literal(name)),
                index: Some(Box::new(Span::Literal(op.to_string()))),
                op: ExpansionOp::Indices(op),
                quoted: false,
            }
        })

        | (sym('#') * parameter())
        .map(|param|{
            Span::Parameter{
                name: param.0,
                index: param.1,
                op: ExpansionOp::Length,
                quoted: false,
            }
        })

        | (parameter() + (tag("##") | tag("#") | tag("%%") | tag("%")) + word(WordType::CmdWord))
        .map(|((param, op), word)|{
            Span::Parameter{
                name: param.0,
                index: param.1,
                op: ExpansionOp::GetOrAction(op.to_string(), word),
                quoted: false,
            }
        })

        | (parameter() - sym('/') + one_of("/#%").opt() + string_except("/").opt() - sym('/') + string_except("}").opt())
        .map(|(((param, begin), pattern), replacement)|{
            let pattern = match pattern {
                Some(pattern) => pattern,
                None => "".to_string(),
            };
            let replacement = match replacement {
                Some(replacement) => replacement,
                None => "".to_string(),
            };
            Span::Parameter {
                name: param.0,
                index: param.1,
                op: ExpansionOp::Subst {
                    pattern: pattern,
                    replacement: replacement,
                    op: begin,
                },
                quoted: false,
            }
        })

        | (parameter())
        .map(|param| {
            Span::Parameter {
                name: param.0,
                index: param.1,
                op: ExpansionOp::GetOrEmpty,
                quoted: false,
            }
        })

        // TODO
        // ${parameter^pattern}
        // ${parameter^^pattern}
        // ${parameter,pattern}
        // ${parameter,,pattern}
        // ${parameter@operator}

    )
    // * one_of("#!").opt() + expandable_var_name() + array_index().opt() + param_opt.opt()
    - sym('}')
}

fn string_except<'a>(exceptions: &'static str) -> Parser<'a, char, String> {
    (escape_sequence() | none_of(exceptions)).repeat(1..)
    .map(|chars| String::from_iter(chars))
    .name("string_except")
}

fn array_index<'a>() -> Parser<'a, char, Span> {
    sym('[')
    * (
        one_of("*@").map(|c| Span::Literal(c.to_string()))
        | expr().map(|expr| Span::ArithExpr {expr:expr})
        | command_substitution_span()
    )
    - sym(']')
}

fn parameter<'a>() -> Parser<'a, char, (Box<Span>, Option<Box<Span>>)> {
    (var_name() + array_index().opt())
    .map(|(name, idx)|{
        (Box::new(Span::Literal(name)), idx.map(|i| Box::new(i)))
        // if let Some(idx) = idx {
        //     Span::ArrayParameter {
        //         name: name,
        //         index: Box::new(idx),
        //         quoted: false,
        //     }
        // } else {
        //     Span::Literal(name)
        // }
    })

    | (one_of("?$!*@#-") | is_a(|c: char| c.is_ascii_digit())).map(|c| (Box::new(Span::Literal(c.to_string())), None))

}

// expandable_var_name = { var_name | special_var_name }
fn expandable_var_name<'a>() -> Parser<'a, char, String> {
    var_name()
    | (one_of("?$!*@#-") | is_a(|c: char| c.is_ascii_digit())).map(|c| c.to_string())
}

fn var_name<'a>() -> Parser<'a, char, String> {
    is_a(|c: char| c == '_' || c.is_ascii_alphanumeric()).repeat(1..)
    .map(|chars| {
        String::from_iter(chars.into_iter())
    })
    .name("var_name")
}

fn initializer<'a>() -> Parser<'a, char, Initializer> {
    (sym('(') * space() * (word(WordType::CmdWord) - space()).repeat(0..) - sym(')'))
    .map(|words| Initializer::Array(words))

    | word(WordType::CmdWord).map(|w| Initializer::String(w))

    | space().map(|_| Initializer::String(Word(vec![])))
}


fn compound_list<'a>() -> Parser<'a, char, Vec<Term>> {
    Parser::new(move |input: Arc<dyn Input<char>>, start: usize| {
        (COMPOUND_LIST.method)(input.clone(), start)
    })
}

// compound_list    : linebreak term
//                  | linebreak term separator
//                  ;
fn _compound_list<'a>() -> Parser<'a, char, Vec<Term>> {
    // (linebreak() * term() + separator().opt())
    (term() + separator().opt())
    .map(|(mut terms, bg)| {
        if let Some(bg) = bg {
            if let Some(mut last) = terms.last_mut() {
                last.background = bg;
            }
        }

        terms
    })
    .expect("compound list")
}

// term             : term separator and_or
//                  |                and_or
//                  ;
fn term<'a>() -> Parser<'a, char, Vec<Term>> {
    let others = separator() + and_or();
    (and_or() + others.repeat(0..)).map(|(term, terms)|
    {
        let mut res = vec![term];
        terms.into_iter().for_each(|(bg, term)| {
            res.last_mut().unwrap().background = bg;
            res.push(term);
        });
        res
    })
}


// subshell         : '(' compound_list ')'
//                  ;
fn subshell<'a>() -> Parser<'a, char, Vec<Term>> {
    sym('(') * space() * compound_list() - sym(')').expect(")")
}

// command_substitution_span = { "$" ~ subshell_group }
fn command_substitution_span<'a>() -> Parser<'a, char, Span> {
    sym('$') * subshell()
    .map(|terms|
        Span::Command{
            body: terms,
            quoted: false,
        }
    )
}


// assignment = ${ var_name ~ index ~ assignment_op ~ initializer ~ WHITESPACE? }
fn assignment<'a>() -> Parser<'a, char, Assignment> {
    (var_name()
    + (sym('[') * expr() - sym(']')).opt()
    + (tag("=").map(|_| false) | tag("+=").map(|_| true))
    + initializer()
    - space())
    .map(|(((name, idx), append), init)| {
        Assignment {
            name: name,
            index: idx,
            append: append,
            initializer: init,
        }
    })
}

// cmd_prefix       :            io_redirect
//                  | cmd_prefix io_redirect
//                  |            ASSIGNMENT_WORD
//                  | cmd_prefix ASSIGNMENT_WORD
//                  ;
fn cmd_prefix<'a>() -> Parser<'a, char, (Vec<Assignment>, Vec<Redirection>)> {
    (
        assignment().map(|assignment| (Some(assignment), None)) |
        io_redirect().map(|redirect| (None, Some(redirect)))
    ).repeat(0..)
    .map(|v| {
        let mut assignments = Vec::new();
        let mut redirects = Vec::new();

        v.into_iter().for_each(|(a, r)| {
            if let Some(a) = a {
                assignments.push(a);
            }
            if let Some(mut r) = r {
                redirects.append(&mut r)
            }
        });

        (assignments, redirects)
    })
}

// cmd_word         : WORD                   /* Apply rule 7b */
//                  ;
fn cmd_word<'a>() -> Parser<'a, char, Word> {
    // !reserved_word() * word(WordType::CmdWord)
    word(WordType::CmdWord)
}

// cmd_name         : WORD                   /* Apply rule 7a */
//                  ;
fn cmd_name<'a>() -> Parser<'a, char, (bool, Word)> {
    (sym('\\').opt() + (!reserved_word() * word(WordType::CmdName)))
    .map(|(opt, word)| {
        (opt.is_some(), word)
    })
    | (sym('\"') * (!reserved_word() * word(WordType::CmdName)) - sym('\"'))
    .map(|word| {
        (true, word)
    })
}

fn heredoc_quoted_line<'a>(trim: bool) -> Parser<'a, char, Word> {
    let trim = if trim {
        space()
    } else {
        empty()
    };

    (
        trim * none_of("\n").repeat(0..) - sym('\n')
    ).map(|chars| {
        Word(vec![Span::Literal(String::from_iter(chars))])
    })
}

fn _heredoc_line<'a>() -> Parser<'a, char, Vec<Span>> {
    (
        param_span()
        | param_ex_span()
        | expr_span()
        | command_span()
        | backtick_span()
        | command_substitution_span()
        | none_of("\n").map(|c| Span::Literal(c.to_string()))
    ).repeat(0..)
    - sym('\n')
}

fn heredoc_line<'a>(trim: bool) -> Parser<'a, char, Word> {
    let trim = if trim {
        space()
    } else {
        empty()
    };

    (
        trim *
        Parser::new(move |input: Arc<dyn Input<char>>, start: usize| {
            (HEREDOC_LINE.method)(input.clone(), start)
        })
    ).map(|spans| {
        let mut res = vec![];
        let mut literal = String::new();

        spans.into_iter().for_each(|s| {
            match s {
                Span::Literal(l) => literal.push_str(&l),
                _ => {
                    if literal.len() > 0 {
                        res.push(Span::Literal(literal.clone()));
                        literal = String::new();
                    }
                    res.push(s);
                }
            }
        });

        if literal.len() > 0 {
            res.push(Span::Literal(literal));
        }

        Word(res)
    })
}

// simple_command   : cmd_prefix cmd_word cmd_suffix
//                  | cmd_prefix cmd_word
//                  | cmd_prefix
//                  | cmd_name cmd_suffix
//                  | cmd_name
//                  ;
fn simple_command<'a>() -> Parser<'a, char, Command> {
    (cmd_prefix() + cmd_name() + cmd_suffix())
    .custom_parser(|(((assignments, redirects_p), (ext, name)), (mut words, mut redirects_s)), input, mut pos| {
        let mut argv = vec![name];
        argv.append(&mut words);

        let mut redirects = redirects_p;
        redirects.append(&mut redirects_s);

        let mut heredocs = vec![];
        redirects.iter_mut().for_each(|r| {
            match &r.target {
                RedirectionType::UnresolvedHereDoc {..} => {
                    heredocs.push(r);
                }
                _ => {}
            }
        });

        for heredoc in heredocs {
            if let RedirectionType::UnresolvedHereDoc { delimiter, quoted, trim } = &heredoc.target {
                let delimiter = delimiter.to_string();

                let line_parser = if *quoted {
                    heredoc_quoted_line(*trim)
                }
                else {
                    heredoc_line(*trim)
                };

                let parser = space() * sym('\n') *
                    (!tag(&delimiter) * line_parser).repeat(0..)
                    - tag(&delimiter).expect(&delimiter) - sym('\n');

                let res = parser.parse_at(input.clone(), pos)?;

                heredoc.target = RedirectionType::HereDoc(HereDoc(res.0));
                pos = res.1 - 1;    // The last '\n' should be parsed by outsize of this function.
            }
        }

        Ok((Command::SimpleCommand {
            external: ext,
            argv: argv,
            redirects: redirects,
            assignments: assignments,
        }, pos))
    })
}

fn assignment_command<'a>() -> Parser<'a, char, Command> {
    assignment().repeat(1..)
    .map(|assignments| Command::Assignment { assignments: assignments })
    .name("assignment_command")
}

// redirect_list    :               io_redirect
//                  | redirect_list io_redirect
//                  ;
// fn redirect_list<'a>() -> Parser<'a, char,

fn command<'a>() -> Parser<'a, char, Command> {
    space() *
    (
        compound_command()
        // | compound_command() + redirect_list()
        | function_definition()
        | local_definition()
        | return_command()
        | break_command()
        | continue_command()
        | simple_command()
        | assignment_command()
    )
}

//local_definition = { "local" ~ (assignment | var_name)+ }
fn local_definition<'a>() -> Parser<'a, char, Command> {
    (
        tag("local") * space() *
        (
            assignment().map(|assign| {
                LocalDeclaration::Assignment(assign)
            })
            | var_name().map(|name| {
                LocalDeclaration::Name(name)
            })
            - space()
        ).repeat(1..)
    ).map(|declarations| {
        Command::LocalDef {
            declarations: declarations
        }
    })
    .name("local_definition")
}


// function_definition : fname '(' ')' linebreak function_body
fn function_definition<'a>() -> Parser<'a, char, Command> {
    (
        (
            (tag("function") * space() * var_name() - space() - tag("()").opt())
            | (var_name() - tag("()"))
        )
        - linebreak() + function_body().expect("function body")
    )
    .map(|(name, body)| {
        Command::FunctionDef {
            name: name,
            body: Box::new(body),
        }
    })
}

// function_body    : compound_command                /* Apply rule 9 */
//                  | compound_command redirect_list  /* Apply rule 9 */
//                  ;
fn function_body<'a>() -> Parser<'a, char, Command> {
    compound_command() // + redirect_list()
}

fn condition_clause<'a>() -> Parser<'a, char, Command> {
    (tag("[[") * space() * sym('!').opt() - space() + cond_expr() - space() - tag("]]"))
    .map(|(is_not, expr)| {
        Command::Cond {
            is_not: is_not.is_some(),
            expr: Some(expr),
        }
    })
}

fn cond_expr<'a>() -> Parser<'a, char, Box<CondExpr>> {
    (cond_and() - space() + (tag("||") * space() * call(cond_expr) - space()).opt())
    .map(|(lhs, rhs)| {
        if let Some(rhs) = rhs {
            Box::new(CondExpr::Or(lhs, rhs))
        } else {
            lhs
        }
    })
}

fn cond_and<'a>() -> Parser<'a, char, Box<CondExpr>> {
    (cond_term() - space() + (tag("&&") * space() * call(cond_and) - space()).opt())
    .map(|(lhs, rhs)| {
        if let Some(rhs) = rhs {
            Box::new(CondExpr::And(lhs, rhs))
        } else {
            lhs
        }
    })
}

fn cond_term<'a>() -> Parser<'a, char, Box<CondExpr>> {
    let primary_op = sym('-') * one_of("znabcdefghkprstuwxOGLNS");
     // TODO: -nt -ot -ef
    let cond_op = tag("==") | tag("=") | tag("!=") | tag(">") | tag("<")
                | tag("-gt") | tag("-lt") | tag("-ge") | tag("-le") | tag("-eq") | tag("-ne");

    (
        (primary_op - space() + cond_primary())
        .convert(|(op, expr)| {
            match op {
                'z' => Ok(Box::new(CondExpr::StrEq(expr, Box::new(CondExpr::Word(Word(vec![Span::Literal("".into())])))))),
                'n' => Ok(Box::new(CondExpr::StrNe(expr, Box::new(CondExpr::Word(Word(vec![Span::Literal("".into())])))))),
                'a' | 'd' | 'e' | 'f' | 'h' | 's' | 'L' | 'N' => Ok(Box::new(CondExpr::File(expr, format!("-{}", op)))),
                _ => Err(format!("-{} is not supported yet.", op)),
            }
        })

        | (cond_primary() - space() - tag("=~") - space() + regex_pattern())
        .map(|(expr, pattern)| {
            Box::new(CondExpr::Regex(expr, pattern))
        })

        | (cond_primary() - space() + (cond_op - space() + call(cond_term) - space()).opt())
        .map(|(lhs, term)| {
            if let Some((op, rhs)) = term {
                match op {
                    "-eq" => Box::new(CondExpr::Eq(lhs, rhs)),
                    "-ne" => Box::new(CondExpr::Ne(lhs, rhs)),
                    "-lt" | "<" => Box::new(CondExpr::Lt(lhs, rhs)),
                    "-le" => Box::new(CondExpr::Le(lhs, rhs)),
                    "-gt" | ">" => Box::new(CondExpr::Gt(lhs, rhs)),
                    "-ge" => Box::new(CondExpr::Ge(lhs, rhs)),
                    "==" | "=" => Box::new(CondExpr::StrEq(lhs, rhs)),
                    "!=" => Box::new(CondExpr::StrNe(lhs, rhs)),
                    _ => unreachable!(),
                }
            } else {
                lhs
            }
        })
    )

    - space()
}

// cond_primary_inner = _{
//     word
//     | "(" ~ cond_expr ~ ")"
// }
fn cond_primary<'a>() -> Parser<'a, char, Box<CondExpr>> {
    word(WordType::CmdWord).map(|w| Box::new(CondExpr::Word(w)))

    | (sym('(') * space() * call(cond_expr) - space() - sym(')'))
}

fn regex_pattern<'a>() -> Parser<'a, char, String> {
    let quoted = sym('\"') * none_of("\"").repeat(0..).map(|chars| String::from_iter(chars)) - sym('\"');
    (quoted | string_except(" \t\r\n\"")).repeat(1..)
    .map(|patterns| {
        patterns.join("")
    })
    .name("regex_pattern")
}

fn compound_command<'a>() -> Parser<'a, char, Command> {
    brace_group()
    | if_clause()
    | while_clause()
    | for_clause()
    | subshell().map(|terms| Command::SubShellGroup {
        terms: terms
    })
    | case_clause()
    | condition_clause()
}

// case_clause      : Case WORD linebreak in linebreak case_list    Esac
//                  | Case WORD linebreak in linebreak case_list_ns Esac
//                  | Case WORD linebreak in linebreak              Esac
//                  ;
fn case_clause<'a>() -> Parser<'a, char, Command> {
    (
        tag("case") * space() * cmd_word().expect("word") - linebreak() - tag("in").expect("in") - linebreak()
        + (case_list() | case_list_ns()).opt() - space() * tag("esac").expect("esac")
    ).map(|(word, cases)| {
        Command::Case {
            word: word,
            cases: if let Some(cases) = cases { cases } else { vec![] },
        }
    })
}

// case_list        : case_list case_item
//                  |           case_item
//                  ;
fn case_list<'a>() -> Parser<'a, char, Vec<CaseItem>> {
    case_item().repeat(1..)
    .name("case_list")
}

// case_list_ns     : case_list case_item_ns
//                  |           case_item_ns
//                  ;
fn case_list_ns<'a>() -> Parser<'a, char, Vec<CaseItem>> {
    (case_list().opt() + case_item_ns())
    .map(|(items, item_ns)| {
        if let Some(mut items) = items {
            items.push(item_ns);
            items
        } else {
            vec![item_ns]
        }
    })
}

// case_item_ns     :     pattern ')'
//                  |     pattern ')' compound_list
//                  | '(' pattern ')' linebreak
//                  | '(' pattern ')' compound_list
//                  ;
fn case_item_ns<'a>() -> Parser<'a, char, CaseItem> {
    (
        (sym('(') * space()).opt()
        * pattern() - sym(')') - linebreak()
        + compound_list().opt() - linebreak().opt()
    ).map(|(patterns, terms)| {
        CaseItem {
            patterns: patterns,
            body: terms.unwrap_or(vec![]),
        }
    })
}

// case_item        :     pattern ')' linebreak     DSEMI linebreak
//                  |     pattern ')' compound_list DSEMI linebreak
//                  | '(' pattern ')' linebreak     DSEMI linebreak
//                  | '(' pattern ')' compound_list DSEMI linebreak
//                  ;
fn case_item<'a>() -> Parser<'a, char, CaseItem> {
    (
        space() * (sym('(') * space()).opt()
        * pattern() - sym(')').expect(")") - linebreak()
        - space() + compound_list().opt() - linebreak().opt()
        - space() - tag(";;").expect(";;") - linebreak()
    ).map(|(patterns, terms)| {
        CaseItem {
            patterns: patterns,
            body: terms.unwrap_or(vec![]),
        }
    })
}

// pattern          :             WORD         /* Apply rule 4 */
//                  | pattern '|' WORD         /* Do not apply rule 4 */
//                  ;
fn pattern<'a>() -> Parser<'a, char, Vec<Word>> {
    !tag("esac")
    * (word(WordType::CmdWord) + (sym('|') * space() * word(WordType::CmdWord)).repeat(0..))
    .map(|(word, mut others)| {
        let mut res = vec![word];
        res.append(&mut others);
        res
    })
}

fn brace_group<'a>() -> Parser<'a, char, Command> {
    (
        space() * sym('{') * linebreak()
        * compound_list()
        - space() * sym('}').expect("}")
    )
    .map(|terms| Command::Group { terms: terms })
}

//
//  Return/Break/Continue Command
//
// return_command = { "return" ~ (num | param_span)? }
fn return_command<'a>() -> Parser<'a, char, Command> {
    tag("return") * space()
    * (
        num().map(|n| Word(vec![Span::Literal(n)]))
        | param_span().map(|span| Word(vec![span]))
    ).opt().map(|w|
        Command::Return{ status: w }
    )
}

// break_command = { "break" }
fn break_command<'a>() -> Parser<'a, char, Command> {
    tag("break").map(|_| Command::Break)
}

// continue_command = { "continue" }
fn continue_command<'a>() -> Parser<'a, char, Command> {
    tag("continue").map(|_| Command::Continue)
}


// for_clause       : For name                                      do_group
//                  | For name                       sequential_sep do_group
//                  | For name linebreak in          sequential_sep do_group
//                  | For name linebreak in wordlist sequential_sep do_group
//                  ;
fn for_clause<'a>() -> Parser<'a, char, Command> {
    tag("for") * space() *
    (
        // arith_for_exprs = { "((" ~ expr ~";" ~ expr ~ ";" ~ expr ~ "))" }
        // arith_for_command = {
        //     "for" ~ arith_for_exprs ~ (";" | wsnl)+ ~ "do" ~ compound_list ~ "done"
        // }
        (
            tag("((") * space() * expr() - sym(';').expect(";") - space()
            + expr() - sym(';').expect(";") - space() + expr()
            - tag("))").expect("))") - separator()
            + do_group()
        ).map(|(((init, cond), update), body)| {
            Command::ArithFor {
                init,
                cond,
                update,
                body,
            }
        })

        | (var_name() - separator() + do_group())
        .map(|(name, terms)| {
            Command::For {
                var_name: name,
                words: vec![],
                body: terms,
            }
        })

        | (var_name() - linebreak() - tag("in") + (space() * cmd_word()).repeat(0..) - separator() + do_group())
        .map(|((name, words), terms)| {
            Command::For {
                var_name: name,
                words: words,
                body: terms,
            }
        })
    ).expect("for clause")
}

// do_group         : Do compound_list Done           /* Apply rule 6 */
fn do_group<'a>() -> Parser<'a, char, Vec<Term>> {
    space() * tag("do") * linebreak() * compound_list() - space() * tag("done").expect("done")
}

// if_clause        : If compound_list Then compound_list else_part Fi
//                  | If compound_list Then compound_list           Fi
//                  ;
fn if_clause<'a>() -> Parser<'a, char, Command> {
    (
        tag("if") * linebreak() * compound_list()
        - space() * tag("then").expect("then") - linebreak() + compound_list()
        + elif_part().repeat(0..)
        + else_part().opt()
        - space() - tag("fi").expect("fi")
    ).map(|(((condition, then_part), elif_parts), else_part)|{
        Command::If {
            condition: condition,
            then_part: then_part,
            elif_parts: elif_parts,
            else_part: else_part,
            redirects: vec![],
        }
    })
}

// elif_part = { "elif" ~ compound_list ~ "then" ~ compound_list }
fn elif_part<'a>() -> Parser<'a, char, ElIf> {
    (
        space() * tag("elif") * linebreak() * compound_list() -
        space() - tag("then").expect("then") - linebreak() + compound_list()
    ).map(|(condition, then_part)| {
        ElIf {
            condition: condition,
            then_part: then_part
        }
    })
}

// else_part = { "else" ~ compound_list }
fn else_part<'a>() -> Parser<'a, char, Vec<Term>> {
    space() * tag("else") * linebreak() * compound_list()
}

// while_command = {
//     "while" ~ compound_list ~ "do" ~ compound_list ~ "done"
// }
fn while_clause<'a>() -> Parser<'a, char, Command> {
    (tag("while") * linebreak() * compound_list() + do_group().expect("do group"))
    .map(|(condition, body)| {
        Command::While {
            condition: condition,
            body: body
        }
    })
}

// pipe_sequence    :                             command
//                  | pipe_sequence '|' linebreak command
//                  ;
fn pipeline<'a>() -> Parser<'a, char, Pipeline> {
    let cmds = space() * sym('|') * linebreak() * command();

    (command().map(|cmd| Pipeline {
        run_if: RunIf::Always,
        commands: vec![cmd],
    }) + cmds.repeat(0..))
    .map(|(mut pipe, mut cmds)| {
        pipe.commands.append(&mut cmds);
        pipe
    })
}

// and_or           :                         pipeline
//                  | and_or AND_IF linebreak pipeline
//                  | and_or OR_IF  linebreak pipeline
//                  ;
fn and_or<'a>() -> Parser<'a, char, Term> {
    let others = space() * ((tag("||").map(|_| RunIf::Failure) | tag("&&").map(|_| RunIf::Success)) - linebreak() + pipeline())
        .map(|(run_if, mut pipe)| { pipe.run_if = run_if; pipe } );

    space() *
    (pipeline() + others.repeat(0..))
    .map_collect(|(pipe, mut others), input| {
        let mut pipelines = vec![pipe];
        pipelines.append(&mut others);

        Term {
            code: String::from_iter(input),
            pipelines: pipelines,
            background: false,
        }
    })
}

// list             : list separator_op and_or
//                  |                   and_or
//                  ;
fn list<'a>() -> Parser<'a, char, Vec<Term>> {
    let others = separator_op() + and_or();
    (and_or() + others.repeat(0..)).map(|(term, terms)|
    {
        let mut res = vec![term];
        terms.into_iter().for_each(|(bg, term)| {
            res.last_mut().unwrap().background = bg;
            res.push(term);
        });
        // res.append(&mut terms);
        res
    })
}

// complete_command : list separator_op
//                  | list
//                  ;
fn complete_command<'a>() -> Parser<'a, char, Vec<Term>> {
    (list() + separator_op().opt()).map(|(mut terms, bg)| {
        if let Some(bg) = bg {
            terms.last_mut().unwrap().background = bg;
        }
        terms
    })
}

// complete_commands: complete_commands newline_list complete_command
//                  |                                complete_command
//                  ;
fn complete_commands<'a>() -> Parser<'a, char, Vec<Term>> {
    let cmds = newline_list() * complete_command();

    // newline_list().map(|_| vec![]) |
    // linebreak() *
    (complete_command() + cmds.repeat(0..).map(|cmds| cmds.concat())).map(|(mut c1, mut c2)| { c1.append(&mut c2); c1 })
}

// program          : linebreak complete_commands linebreak
//                  | linebreak
//                  ;
fn program<'a>() -> Parser<'a, char, Ast> {
    (linebreak() * complete_commands() - linebreak()).map(|terms| Ast{ terms: terms })
    | newline_list().map(|_| Ast{ terms: vec![]})
}

lazy_static! {
    pub static ref PROGRAM: Parser<'static, char, Ast> = program();
    pub static ref COMPOUND_LIST: Parser<'static, char, Vec<Term>> = _compound_list();
    pub static ref EXPR: Parser<'static, char, Box<Expr>> = _expr();
    pub static ref PARAM_EX_SPAN: Parser<'static, char, Span> = _param_ex_span();
    pub static ref HEREDOC_LINE: Parser<'static, char, Vec<Span>> = _heredoc_line();
}

pub fn error_convert(input: &Vec<char>, err: pom::Error) -> ParseError {
    let (message, position) = match err {
        pom::Error::Repeat { ref message, position, .. } => (message.to_string(), position),
        pom::Error::Mismatch { ref message, position } => (message.to_string(), position),
        pom::Error::Conversion { ref message, position } => (message.to_string(), position),
        pom::Error::Expect { ref message, position, .. } => (message.to_string(), position),
        pom::Error::Custom { ref message, position, .. } => (message.to_string(), position),
        pom::Error::Incomplete => ("Incomplete".to_string(), input.len()),
    };

    // Search 2 lines earlier from the current location
    let mut count = 2;
    let mut begin = position;
    let mut line_start = position;
    for i in (0..position).rev() {
        if input[i] == '\n' {
            count -= 1;
            if count == 0 {
                begin = i + 1;
                break;
            }
        } else if count == 2 {
            line_start -= 1;
        }
    }
    if count != 0 {
        begin = 0;
    }

    // Search after 2 lines from the current location
    let mut end = position;
    for i in position .. input.len() {
        if input[i] == '\n' {
            end = i;
            break;
        }
    }
    if end == position {
        end = input.len();
    }

    let mut sources = String::new();
    let mut start = begin;
    for i in begin .. end {
        if input[i] == '\n' {
            sources.push_str("  | ");
            sources.push_str(&String::from_iter(&input[start .. i]));
            sources.push('\n');
            start = i + 1;
        }
    }
    if start < end {
        sources.push_str("  | ");
        sources.push_str(&String::from_iter(&input[start .. end]));
        sources.push('\n');
    }

    let message = format!("{}\n  |\n{}  | {}^---\n  |",
        message, sources, " ".repeat(position - line_start)
    );

    match err {
        pom::Error::Expect { .. } => {
            ParseError::Expected (message)
        }
        _ => {
            ParseError::Fatal (message)
        }
    }
}


pub struct ShellParser {
}

impl ShellParser {
    pub fn new() -> ShellParser {
        ShellParser {
        }
    }

    pub fn parse(&self, script: &str) -> Result<Ast, ParseError> {
        let mut input: Vec<char> = script.chars().collect();

        match input.last() {
            Some(ch) if *ch == '\n' => {}
            _ => input.push('\n'),
        }

        match PROGRAM.parse(Arc::new(InputV {
            input: input.clone()
        })) {
            Ok(ast) => {
                if ast.terms.len() == 0 {
                    Err(ParseError::Empty)
                } else {
                    Ok(ast)
                }
            },
            Err(err) => Err(error_convert(&input, err))
        }
    }
}


#[allow(unused)]
macro_rules! literal_word_vec {
    ($($x:expr), *) => {
        vec![$( Word(vec![Span::Literal($x.to_string())]), )*]
    };
}

#[allow(unused)]
macro_rules! lit {
    ($x:expr) => {
        Word(vec![Span::Literal($x.to_string())])
    };
}

#[allow(unused)]
macro_rules! param {
    ($name:expr, $op:expr, $quoted:expr) => {
        Word(vec![Span::Parameter {
            name: Box::new(Span::Literal($name.to_string())),
            op: $op,
            index: None,
            quoted: $quoted,
        }])
    };
}

#[test]
fn test_debug() {
    // let parser = ShellParser::new();

}

#[test]
fn test_extra() {
    let parser = ShellParser::new();

    assert_eq!(
        parser.parse(r#"
for f in `test -d device && find -L device -maxdepth 4 -name 'vendorsetup.sh' 2> /dev/null | sort` \
         `test -d vendor && find -L vendor -maxdepth 4 -name 'vendorsetup.sh' 2> /dev/null | sort`
do
    echo "including $f"
    . $f
done
        "#),
        Ok(Ast { terms: vec![Term {
            code: "for f in `test -d device && find -L device -maxdepth 4 -name \'vendorsetup.sh\' 2> /dev/null | sort` \\\n         `test -d vendor && find -L vendor -maxdepth 4 -name \'vendorsetup.sh\' 2> /dev/null | sort`\ndo\n    echo \"including $f\"\n    . $f\ndone".into(),
            pipelines: vec![Pipeline {
                run_if: RunIf::Always,
                commands: vec![Command::For {
                    var_name: "f".to_string(),
                    words: vec![Word(vec![Span::Command {
                        body: vec![Term {
                            code: "test -d device && find -L device -maxdepth 4 -name \'vendorsetup.sh\' 2> /dev/null | sort".into(),
                            pipelines: vec![Pipeline {
                                run_if: RunIf::Always,
                                commands: vec![Command::SimpleCommand {
                                    external: false,
                                    argv: vec![lit!("test"), lit!("-d"), lit!("device")],
                                    redirects: vec![],
                                    assignments: vec![]
                                }]
                            }, Pipeline {
                                run_if: RunIf::Success,
                                commands: vec![Command::SimpleCommand {
                                    external: false,
                                    argv: vec![lit!("find"), lit!("-L"), lit!("device"), lit!("-maxdepth"), lit!("4"), lit!("-name"), lit!("vendorsetup.sh")],
                                    redirects: vec![Redirection {
                                        fd: 2,
                                        direction: RedirectionDirection::Output,
                                        target: RedirectionType::File(lit!("/dev/null"))
                                    }],
                                    assignments: vec![]
                                }, Command::SimpleCommand {
                                    external: false,
                                    argv: vec![lit!("sort")],
                                    redirects: vec![],
                                    assignments: vec![]
                                }]
                            }],
                            background: false
                        }],
                        quoted: false
                    }]), Word(vec![Span::Command {
                        body: vec![Term {
                            code: "test -d vendor && find -L vendor -maxdepth 4 -name \'vendorsetup.sh\' 2> /dev/null | sort".to_string(),
                            pipelines: vec![Pipeline {
                                run_if: RunIf::Always,
                                commands: vec![Command::SimpleCommand {
                                    external: false,
                                    argv: vec![lit!("test"), lit!("-d"), lit!("vendor")],
                                    redirects: vec![],
                                    assignments: vec![]
                                }] }, Pipeline {
                                    run_if: RunIf::Success,
                                    commands: vec![Command::SimpleCommand {
                                        external: false,
                                        argv: vec![lit!("find"), lit!("-L"), lit!("vendor"), lit!("-maxdepth"), lit!("4"), lit!("-name"), lit!("vendorsetup.sh")],
                                        redirects: vec![Redirection {
                                            fd: 2,
                                            direction: RedirectionDirection::Output,
                                            target: RedirectionType::File(lit!("/dev/null"))
                                        }],
                                        assignments: vec![]
                                    }, Command::SimpleCommand {
                                        external: false,
                                        argv: vec![lit!("sort")],
                                        redirects: vec![],
                                        assignments: vec![]
                                    }]
                                }],
                                background: false
                            }],
                            quoted: false
                        }])],
                    body: vec![Term {
                        code: "echo \"including $f\"".into(),
                        pipelines: vec![Pipeline {
                            run_if: RunIf::Always,
                            commands: vec![Command::SimpleCommand {
                                external: false,
                                argv: vec![lit!("echo"),
                                    Word(vec![Span::Literal("including ".into()),
                                        Span::Parameter {
                                            name: Box::new(Span::Literal("f".into())),
                                            index: None,
                                            op: ExpansionOp::GetOrEmpty,
                                            quoted: true
                                        }])],
                                redirects: vec![],
                                assignments: vec![]
                            }]
                        }],
                        background: false
                    }, Term {
                        code: ". $f".to_string(),
                        pipelines: vec![Pipeline {
                            run_if: RunIf::Always,
                            commands: vec![Command::SimpleCommand {
                                external: false,
                                argv: vec![lit!("."), Word(vec![Span::Parameter {
                                    name: Box::new(Span::Literal("f".into())),
                                    index: None,
                                    op: ExpansionOp::GetOrEmpty,
                                    quoted: false
                                }])],
                                redirects: vec![],
                                assignments: vec![]
                            }]
                        }],
                        background: false
                    }]
                }]
            }],
            background: false
        }] })
    );

    assert_eq!(
        parser.parse(r#"
    while [ ]; do
        T=`PWD= /bin/pwd`
        \cd ..
    done
        "#),
        Ok(Ast { terms: vec![Term {
            code: "while [ ]; do\n        T=`PWD= /bin/pwd`\n        \\cd ..\n    done".into(),
            pipelines: vec![Pipeline {
                run_if: RunIf::Always,
                commands: vec![Command::While {
                    condition: vec![Term {
                        code: "[ ]".into(),
                        pipelines: vec![Pipeline {
                            run_if: RunIf::Always,
                            commands: vec![Command::SimpleCommand {
                                external: false,
                                argv: vec![lit!("["), lit!("]")],
                                redirects: vec![],
                                assignments: vec![]
                            }]
                        }],
                        background: false
                    }],
                    body: vec![Term {
                        code: "T=`PWD= /bin/pwd`".into(),
                        pipelines: vec![Pipeline {
                            run_if: RunIf::Always,
                            commands: vec![Command::Assignment {
                                assignments: vec![Assignment {
                                    name: "T".into(),
                                    initializer: Initializer::String(Word(vec![Span::Command {
                                        body: vec![Term {
                                            code: "PWD= /bin/pwd".into(),
                                            pipelines: vec![Pipeline {
                                                run_if: RunIf::Always,
                                                commands: vec![Command::SimpleCommand {
                                                    external: false,
                                                    argv: vec![lit!("/bin/pwd")],
                                                    redirects: vec![],
                                                    assignments: vec![Assignment {
                                                        name: "PWD".into(),
                                                        initializer: Initializer::String(Word(vec![])),
                                                        index: None,
                                                        append: false
                                                    }]
                                                }]
                                            }],
                                            background: false
                                        }],
                                        quoted: false
                                    }])),
                                    index: None,
                                    append: false
                                }]
                            }]
                        }],
                        background: false
                    },
                    Term {
                        code: "\\cd ..".into(),
                        pipelines: vec![Pipeline {
                            run_if: RunIf::Always,
                            commands: vec![Command::SimpleCommand {
                                external: true,
                                argv: vec![lit!("cd"), lit!("..")],
                                redirects: vec![],
                                assignments: vec![]
                            }]
                        }],
                        background: false
                    }]
                }]
            }],
            background: false
        }] }),
    );

    assert_eq!(
        parser.parse(r#"
        echo "\
$T/prebuilts/misc/linux-x86/analyzer/tools/scan-build/scan-build \
--top=$T"
        "#),
        Ok(Ast { terms: vec![Term {
            code: "echo \"\\\n$T/prebuilts/misc/linux-x86/analyzer/tools/scan-build/scan-build \\\n--top=$T\"".into(),
            pipelines: vec![Pipeline {
                run_if: RunIf::Always,
                commands: vec![Command::SimpleCommand {
                    external: false,
                    argv: vec![lit!("echo"), Word(vec![
                        Span::Literal("\\\n$T".into()),
                        Span::Literal("/prebuilts/misc/linux-x86/analyzer/tools/scan-build/scan-build --top=".into()),
                        Span::Parameter {
                            name: Box::new(Span::Literal("T".into())),
                            index: None,
                            op: ExpansionOp::GetOrEmpty,
                            quoted: true
                        }])],
                    redirects: vec![],
                    assignments: vec![]
                }]
            }],
            background: false
        }] }),
    );

    assert_eq!(
        parser.parse(r#"
            $(sed -e "s/-.*$//")
        "#),
        Ok(Ast { terms: vec![Term {
            code: "$(sed -e \"s/-.*$//\")".into(),
            pipelines: vec![Pipeline {
                run_if: RunIf::Always,
                commands: vec![Command::SimpleCommand {
                    external: false,
                    argv: vec![Word(vec![Span::Command {
                        body: vec![Term {
                            code: "sed -e \"s/-.*$//\"".into(),
                            pipelines: vec![Pipeline {
                                run_if: RunIf::Always,
                                commands: vec![Command::SimpleCommand {
                                    external: false,
                                    argv: vec![lit!("sed"), lit!("-e"),
                                        Word(vec![Span::Literal("s/-.*".to_string()), Span::Literal("$/".to_string()), Span::Literal("/".to_string())])],
                                    redirects: vec![],
                                    assignments: vec![]
                                }]
                            }],
                            background: false
                        }],
                        quoted: false
                    }])],
                    redirects: vec![],
                    assignments: vec![]
                }]
            }],
            background: false
        }]})
    );


    assert_eq!(
        parser.parse("${VARIANT_CHOICES[$(($ANSWER-1))]}"),
        Ok(Ast { terms: vec![Term {
            code: "${VARIANT_CHOICES[$(($ANSWER-1))]}".into(),
            pipelines: vec![Pipeline {
                run_if: RunIf::Always,
                commands: vec![Command::SimpleCommand {
                    external: false,
                    argv: vec![Word(vec![Span::Parameter {
                        name: Box::new(Span::Literal("VARIANT_CHOICES".into())),
                        index: Some(Box::new(Span::Command {
                            body: vec![Term {
                                code: "($ANSWER-1)".into(),
                                pipelines: vec![Pipeline {
                                    run_if: RunIf::Always,
                                    commands: vec![Command::SubShellGroup {
                                        terms: vec![Term {
                                            code: "$ANSWER-1".into(),
                                            pipelines: vec![Pipeline {
                                                run_if: RunIf::Always,
                                                commands: vec![Command::SimpleCommand {
                                                    external: false,
                                                    argv: vec![Word(vec![Span::Parameter {
                                                            name: Box::new(Span::Literal("ANSWER".into())),
                                                            index: None,
                                                            op: ExpansionOp::GetOrEmpty,
                                                            quoted: false
                                                        },
                                                        Span::Literal("-1".to_string())])],
                                                    redirects: vec![],
                                                    assignments: vec![]
                                                }]
                                            }],
                                            background: false
                                        }]
                                    }]
                                }],
                                background: false
                            }],
                            quoted: false
                        })),
                        op: ExpansionOp::GetOrEmpty,
                        quoted: false
                    }])],
                    redirects: vec![],
                    assignments: vec![]
                }]
            }],
            background: false
        }]})
    );

    assert_eq!(
        parser.parse("fish"),
        Ok(Ast {
            terms: vec![Term {
                code: "fish".to_string(),
                background: false ,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::SimpleCommand {
                        external: false,
                        argv: literal_word_vec!["fish"],
                        redirects: vec![],
                        assignments: vec![]
                    }]
                }],
            }
        ]})
    );

    assert_eq!(
        parser.parse(
r#"# Backslashes should escape the following:
printf '%s\n' "\$\`\"\\\
test"
"#
        ),
        Ok(Ast {
            terms: vec![Term {
                code: "printf \'%s\\n\' \"\\$\\`\\\"\\\\\\\ntest\"".into(),
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::SimpleCommand {
                        external: false,
                        argv: literal_word_vec!["printf", "%s\\n", "$`\"\\test"],
                        redirects: vec![],
                        assignments: vec![]
                    }]
                }],
            }],
        })
    );

    assert_eq!(
        parser.parse(
r#"printf 'arg one: %s; arg two: %s\n' without \
    newline
"#),
        Ok(Ast {
            terms: vec![Term {
                code: "printf \'arg one: %s; arg two: %s\\n\' without \\\n    newline".into(),
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::SimpleCommand {
                        external: false,
                        argv: literal_word_vec!["printf", "arg one: %s; arg two: %s\\n", "without", "newline"],
                        redirects: vec![],
                        assignments: vec![]
                    }]
                }],
            }
        ]})
    );

    assert_eq!(
        parser.parse(
r##"
# 2.2.2 Single quotes
printf '%s\n' '|&;<>()$`\"'
"##
        ),
        Ok(Ast {
            terms: vec![Term {
                code: "printf \'%s\\n\' \'|&;<>()$`\\\"\'".into(),
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::SimpleCommand {
                        external: false,
                        argv: literal_word_vec!["printf", "%s\\n", "|&;<>()$`\\\""],
                        redirects: vec![],
                        assignments: vec![],
                    }]
                }],
            }],
        })
    );
}

#[test]
pub fn test_simple_commands() {
    let parser = ShellParser::new();

    assert_eq!(
        parser.parse("ls -G /tmp\n"),
        Ok(Ast {
            terms: vec![Term {
                code: "ls -G /tmp".into(),
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::SimpleCommand {
                        external: false,
                        argv: literal_word_vec!["ls", "-G", "/tmp"],
                        redirects: vec![],
                        assignments: vec![],
                    }],
                }],
            }],
        })
    );

    assert_eq!(
        parser.parse("echo hello | hexdump -C | date"),
        Ok(Ast {
            terms: vec![Term {
                code: "echo hello | hexdump -C | date".into(),
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![
                        Command::SimpleCommand {
                            external: false,
                            argv: literal_word_vec!["echo", "hello"],
                            redirects: vec![],
                            assignments: vec![],
                        },
                        Command::SimpleCommand {
                            external: false,
                            argv: literal_word_vec!["hexdump", "-C"],
                            redirects: vec![],
                            assignments: vec![],
                        },
                        Command::SimpleCommand {
                            external: false,
                            argv: literal_word_vec!["date"],
                            redirects: vec![],
                            assignments: vec![],
                        },
                    ],
                }],
            }],
        })
    );

    assert_eq!(
        parser.parse("false || false && echo unreachable; echo \\\nreachable"),
        Ok(Ast {
            terms: vec![
                Term {
                    code: "false || false && echo unreachable".into(),
                    background: false,
                    pipelines: vec![
                        Pipeline {
                            run_if: RunIf::Always,
                            commands: vec![Command::SimpleCommand {
                                external: false,
                                argv: literal_word_vec!["false"],
                                redirects: vec![],
                                assignments: vec![],
                            }],
                        },
                        Pipeline {
                            run_if: RunIf::Failure,
                            commands: vec![Command::SimpleCommand {
                                external: false,
                                argv: literal_word_vec!["false"],
                                redirects: vec![],
                                assignments: vec![],
                            }],
                        },
                        Pipeline {
                            run_if: RunIf::Success,
                            commands: vec![Command::SimpleCommand {
                                external: false,
                                argv: literal_word_vec!["echo", "unreachable"],
                                redirects: vec![],
                                assignments: vec![],
                            }],
                        },
                    ],
                },
                Term {
                    code: "echo \\\nreachable".into(),
                    background: false,
                    pipelines: vec![Pipeline {
                        run_if: RunIf::Always,
                        commands: vec![Command::SimpleCommand {
                            external: false,
                            argv: literal_word_vec!["echo", "reachable"],
                            redirects: vec![],
                            assignments: vec![],
                        }],
                    }],
                },
            ],
        })
    );

    assert_eq!(
        parser.parse("echo -n \"Hello world\" from; echo nsh"),
        Ok(Ast {
            terms: vec![
                Term {
                    code: "echo -n \"Hello world\" from".into(),
                    background: false,
                    pipelines: vec![Pipeline {
                        run_if: RunIf::Always,
                        commands: vec![Command::SimpleCommand {
                            external: false,
                            argv: literal_word_vec!["echo", "-n", "Hello world", "from"],
                            redirects: vec![],
                            assignments: vec![],
                        }],
                    }],
                },
                Term {
                    code: "echo nsh".into(),
                    background: false,
                    pipelines: vec![Pipeline {
                        run_if: RunIf::Always,
                        commands: vec![Command::SimpleCommand {
                            external: false,
                            argv: literal_word_vec!["echo", "nsh"],
                            redirects: vec![],
                            assignments: vec![],
                        }],
                    }],
                },
            ],
        })
    );

    assert_eq!(
        parser.parse("echo foo & sleep 1 &\n echo bar; echo baz & echo foo2 &"),
        Ok(Ast {
            terms: vec![
                Term {
                    code: "echo foo ".into(),
                    background: true,
                    pipelines: vec![Pipeline {
                        run_if: RunIf::Always,
                        commands: vec![Command::SimpleCommand {
                            external: false,
                            argv: literal_word_vec!["echo", "foo"],
                            redirects: vec![],
                            assignments: vec![],
                        }],
                    }],
                },
                Term {
                    code: "sleep 1 ".into(),
                    background: true,
                    pipelines: vec![Pipeline {
                        run_if: RunIf::Always,
                        commands: vec![Command::SimpleCommand {
                            external: false,
                            argv: literal_word_vec!["sleep", "1"],
                            redirects: vec![],
                            assignments: vec![],
                        }],
                    }],
                },
                Term {
                    code: "echo bar".into(),
                    background: false,
                    pipelines: vec![Pipeline {
                        run_if: RunIf::Always,
                        commands: vec![Command::SimpleCommand {
                            external: false,
                            argv: literal_word_vec!["echo", "bar"],
                            redirects: vec![],
                            assignments: vec![],
                        }],
                    }],
                },
                Term {
                    code: "echo baz ".into(),
                    background: true,
                    pipelines: vec![Pipeline {
                        run_if: RunIf::Always,
                        commands: vec![Command::SimpleCommand {
                            external: false,
                            argv: literal_word_vec!["echo", "baz"],
                            redirects: vec![],
                            assignments: vec![],
                        }],
                    }],
                },
                Term {
                    code: "echo foo2 ".into(),
                    background: true,
                    pipelines: vec![Pipeline {
                        run_if: RunIf::Always,
                        commands: vec![Command::SimpleCommand {
                            external: false,
                            argv: literal_word_vec!["echo", "foo2"],
                            redirects: vec![],
                            assignments: vec![],
                        }],
                    }],
                },
            ],
        }),
    );

    assert_eq!(
        parser.parse("PORT=1234 RAILS_ENV=production rails s"),
        Ok(Ast {
            terms: vec![Term {
                code: "PORT=1234 RAILS_ENV=production rails s".into(),
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::SimpleCommand {
                        external: false,
                        argv: literal_word_vec!["rails", "s"],
                        redirects: vec![],
                        assignments: vec![
                            Assignment {
                                name: "PORT".into(),
                                initializer: Initializer::String(Word(vec![Span::Literal(
                                    "1234".into()
                                )])),
                                index: None,
                                append: false,
                            },
                            Assignment {
                                name: "RAILS_ENV".into(),
                                initializer: Initializer::String(Word(vec![Span::Literal(
                                    "production".into()
                                )])),
                                index: None,
                                append: false,
                            }
                        ],
                    }],
                }],
            }],
        })
    );

    assert_eq!(
        parser.parse("ls -G <foo.txt >> bar.txt 2> baz.txt 4>&2"),
        Ok(Ast {
            terms: vec![Term {
                code: "ls -G <foo.txt >> bar.txt 2> baz.txt 4>&2".into(),
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::SimpleCommand {
                        external: false,
                        argv: literal_word_vec!["ls", "-G"],
                        redirects: vec![
                            Redirection {
                                direction: RedirectionDirection::Input,
                                fd: 0,
                                target: RedirectionType::File(lit!("foo.txt")),
                            },
                            Redirection {
                                direction: RedirectionDirection::Append,
                                fd: 1,
                                target: RedirectionType::File(lit!("bar.txt")),
                            },
                            Redirection {
                                direction: RedirectionDirection::Output,
                                fd: 2,
                                target: RedirectionType::File(lit!("baz.txt")),
                            },
                            Redirection {
                                direction: RedirectionDirection::Output,
                                fd: 4,
                                target: RedirectionType::FileOrFd(lit!["2"]),
                            },
                        ],
                        assignments: vec![],
                    }],
                }],
            }],
        })
    );

    assert_eq!(
        parser.parse("[ foo = \"foo\" ];"),
        Ok(Ast{
            terms: vec![Term {
                code: "[ foo = \"foo\" ]".into(),
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::SimpleCommand {
                        external: false,
                        argv: literal_word_vec!["[", "foo", "=", "foo", "]"],
                        redirects: vec![],
                        assignments: vec![]
                    }]
                }],
            }]
        })
    );

    assert_eq!(
        parser.parse(concat!(
            "    echo hello\n",
            "    echo world\n")),
        Ok(Ast{
            terms: vec![
                Term {
                    code: "echo hello".to_string(),
                    background: false,
                    pipelines: vec![Pipeline {
                        run_if: RunIf::Always,
                        commands: vec![Command::SimpleCommand {
                            external: false,
                            argv: literal_word_vec!["echo", "hello"],
                            redirects: vec![],
                            assignments: vec![]
                        }]
                    }]
                },
                Term {
                    code: "echo world".to_string(),
                    background: false,
                    pipelines: vec![Pipeline {
                        run_if: RunIf::Always,
                        commands: vec![Command::SimpleCommand {
                            external: false,
                            argv: literal_word_vec!["echo", "world"],
                            redirects: vec![],
                            assignments: vec![]
                        }]
                    }],
                }
            ]
        })
    );

    assert_eq!(
        parser.parse("[ $name = \"mike\" ];"),
        Ok(Ast{
            terms: vec![Term {
                code: "[ $name = \"mike\" ]".into(),
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::SimpleCommand {
                        external: false,
                        argv: vec![lit!("["), param!("name", ExpansionOp::GetOrEmpty, false), lit!("="), lit!("mike"), lit!("]")],
                        redirects: vec![],
                        assignments: vec![]
                    }]
                }],
            }]
        })
    );
}

#[test]
pub fn test_compound_commands() {
    let parser = ShellParser::new();

    assert_eq!(
        parser.parse("if true; then echo it works; fi"),
        Ok(Ast {
            terms: vec![Term {
                code: "if true; then echo it works; fi".into(),
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::If {
                        condition: vec![Term {
                            code: "true".into(),
                            pipelines: vec![Pipeline {
                                run_if: RunIf::Always,
                                commands: vec![Command::SimpleCommand {
                                    external: false,
                                    argv: literal_word_vec!["true"],
                                    redirects: vec![],
                                    assignments: vec![],
                                }],
                            }],
                            background: false,
                        }],
                        then_part: vec![Term {
                            code: "echo it works".into(),
                            pipelines: vec![Pipeline {
                                run_if: RunIf::Always,
                                commands: vec![Command::SimpleCommand {
                                    external: false,
                                    argv: literal_word_vec!["echo", "it", "works"],
                                    redirects: vec![],
                                    assignments: vec![],
                                }],
                            }],
                            background: false,
                        }],
                        elif_parts: vec![],
                        else_part: None,
                        redirects: vec![],
                    }],
                }],
                background: false,
            }],
        })
    );

    assert_eq!(
        parser.parse("    if true; then\n echo it works;\n    else\n echo $PATH\n   fi\n"),
        Ok(Ast {
            terms: vec![Term {
                code: "if true; then\n echo it works;\n    else\n echo $PATH\n   fi".into(),
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::If {
                        condition: vec![Term {
                            code: "true".into(),
                            pipelines: vec![Pipeline {
                                run_if: RunIf::Always,
                                commands: vec![Command::SimpleCommand {
                                    external: false,
                                    argv: literal_word_vec!["true"],
                                    redirects: vec![],
                                    assignments: vec![],
                                }],
                            }],
                            background: false,
                        }],
                        then_part: vec![Term {
                            code: "echo it works".into(),
                            pipelines: vec![Pipeline {
                                run_if: RunIf::Always,
                                commands: vec![Command::SimpleCommand {
                                    external: false,
                                    argv: literal_word_vec!["echo", "it", "works"],
                                    redirects: vec![],
                                    assignments: vec![],
                                }],
                            }],
                            background: false,
                        }],
                        elif_parts: vec![],
                        else_part: Some(vec![Term {
                            code: "echo $PATH".into(),
                            pipelines: vec![Pipeline {
                                run_if: RunIf::Always,
                                commands: vec![Command::SimpleCommand {
                                    external: false,
                                    argv: vec![
                                        lit!("echo"),
                                        param!("PATH", ExpansionOp::GetOrEmpty, false),
                                    ],
                                    redirects: [].to_vec(),
                                    assignments: [].to_vec()
                                }]
                            }],
                            background: false
                        }]),
                        redirects: vec![],
                    }],
                }],
                background: false,
            }],
        })
    );

    assert_eq!(
        parser.parse("while maybe-true;\n   do\n echo \"while loop!\"; \n   done"),
        Ok(Ast {
            terms: vec![Term {
                code: "while maybe-true;\n   do\n echo \"while loop!\"; \n   done".into(),
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::While {
                        condition: vec![Term {
                            code: "maybe-true".into(),
                            pipelines: vec![Pipeline {
                                run_if: RunIf::Always,
                                commands: vec![Command::SimpleCommand {
                                    external: false,
                                    argv: literal_word_vec!["maybe-true"],
                                    redirects: vec![],
                                    assignments: vec![],
                                }],
                            }],
                            background: false,
                        }],
                        body: vec![Term {
                            code: "echo \"while loop!\"".into(),
                            pipelines: vec![Pipeline {
                                run_if: RunIf::Always,
                                commands: vec![Command::SimpleCommand {
                                    external: false,
                                    argv: literal_word_vec!["echo", "while loop!"],
                                    redirects: vec![],
                                    assignments: vec![],
                                }],
                            }],
                            background: false,
                        }],
                    }],
                }],
                background: false,
            }],
        })
    );

    assert_eq!(
        parser.parse(concat!(
            "if [ foo = \"foo\" ];\n",
            "   then\n",
            "    echo hello\n",
            "    echo world\n",
            "   fi"
        )),
        Ok(Ast {
            terms: vec![Term {
                code: "if [ foo = \"foo\" ];\n   then\n    echo hello\n    echo world\n   fi".into(),
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::If {
                        condition: vec![Term {
                            code: "[ foo = \"foo\" ]".into(),
                            pipelines: vec![Pipeline {
                                run_if: RunIf::Always,
                                commands: vec![Command::SimpleCommand {
                                    external: false,
                                    argv: literal_word_vec!["[", "foo", "=", "foo", "]"],
                                    redirects: vec![],
                                    assignments: vec![],
                                }],
                            }],
                            background: false,
                        }],
                        then_part: vec![
                            Term {
                                code: "echo hello".into(),
                                pipelines: vec![Pipeline {
                                    run_if: RunIf::Always,
                                    commands: vec![Command::SimpleCommand {
                                        external: false,
                                        argv: literal_word_vec!["echo", "hello"],
                                        redirects: vec![],
                                        assignments: vec![],
                                    }],
                                }],
                                background: false,
                            },
                            Term {
                                code: "echo world".into(),
                                pipelines: vec![Pipeline {
                                    run_if: RunIf::Always,
                                    commands: vec![Command::SimpleCommand {
                                        external: false,
                                        argv: literal_word_vec!["echo", "world"],
                                        redirects: vec![],
                                        assignments: vec![],
                                    }],
                                }],
                                background: false,
                            },
                        ],
                        elif_parts: vec![],
                        else_part: None,
                        redirects: vec![],
                    }],
                }],
                background: false,
            }],
        })
    );

    assert_eq!(
        parser.parse(concat!(
            "if [ $name = \"john\" ];",
            "then",
            "    echo Hello, John!;",
            "elif [ $name = \"mike\" ];",
            "then",
            "    echo Hello, Mike!;",
            "elif [ $name = \"emily\" ];",
            "then",
            "    echo Hello, Emily!;",
            "else",
            "    echo Hello, stranger!;",
            "fi"
        )),
        Ok(Ast {
            terms: vec![Term {
                code: "if [ $name = \"john\" ];then    echo Hello, John!;elif [ $name = \"mike\" ];then    echo Hello, Mike!;elif [ $name = \"emily\" ];then    echo Hello, Emily!;else    echo Hello, stranger!;fi".into(),
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::If {
                        condition: vec![Term {
                            code: "[ $name = \"john\" ]".into(),
                            pipelines: vec![Pipeline {
                                run_if: RunIf::Always,
                                commands: vec![Command::SimpleCommand {
                                    external: false,
                                    argv: vec![
                                        lit!("["),
                                        param!("name", ExpansionOp::GetOrEmpty, false),
                                        lit!("="),
                                        lit!("john"),
                                        lit!("]"),
                                    ],
                                    redirects: vec![],
                                    assignments: vec![],
                                }],
                            }],
                            background: false,
                        }],
                        then_part: vec![Term {
                            code: "echo Hello, John!".into(),
                            pipelines: vec![Pipeline {
                                run_if: RunIf::Always,
                                commands: vec![Command::SimpleCommand {
                                    external: false,
                                    argv: literal_word_vec!["echo", "Hello,", "John!"],
                                    redirects: vec![],
                                    assignments: vec![],
                                }],
                            }],
                            background: false,
                        }],
                        elif_parts: vec![
                            ElIf {
                                condition: vec![Term {
                                    code: "[ $name = \"mike\" ]".into(),
                                    pipelines: vec![Pipeline {
                                        run_if: RunIf::Always,
                                        commands: vec![Command::SimpleCommand {
                                            external: false,
                                            argv: vec![
                                                lit!("["),
                                                param!("name", ExpansionOp::GetOrEmpty, false),
                                                lit!("="),
                                                lit!("mike"),
                                                lit!("]"),
                                            ],
                                            redirects: vec![],
                                            assignments: vec![],
                                        }],
                                    }],
                                    background: false,
                                }],
                                then_part: vec![Term {
                                    code: "echo Hello, Mike!".into(),
                                    pipelines: vec![Pipeline {
                                        run_if: RunIf::Always,
                                        commands: vec![Command::SimpleCommand {
                                            external: false,
                                            argv: literal_word_vec!["echo", "Hello,", "Mike!"],
                                            redirects: vec![],
                                            assignments: vec![],
                                        }],
                                    }],
                                    background: false,
                                }],
                            },
                            ElIf {
                                condition: vec![Term {
                                    code: "[ $name = \"emily\" ]".into(),
                                    pipelines: vec![Pipeline {
                                        run_if: RunIf::Always,
                                        commands: vec![Command::SimpleCommand {
                                            external: false,
                                            argv: vec![
                                                lit!("["),
                                                param!("name", ExpansionOp::GetOrEmpty, false),
                                                lit!("="),
                                                lit!("emily"),
                                                lit!("]"),
                                            ],
                                            redirects: vec![],
                                            assignments: vec![],
                                        }],
                                    }],
                                    background: false,
                                }],
                                then_part: vec![Term {
                                    code: "echo Hello, Emily!".into(),
                                    pipelines: vec![Pipeline {
                                        run_if: RunIf::Always,
                                        commands: vec![Command::SimpleCommand {
                                            external: false,
                                            argv: literal_word_vec!["echo", "Hello,", "Emily!"],
                                            redirects: vec![],
                                            assignments: vec![],
                                        }],
                                    }],
                                    background: false,
                                }],
                            },
                        ],
                        else_part: Some(vec![Term {
                            code: "echo Hello, stranger!".into(),
                            pipelines: vec![Pipeline {
                                run_if: RunIf::Always,
                                commands: vec![Command::SimpleCommand {
                                    external: false,
                                    argv: literal_word_vec!["echo", "Hello,", "stranger!"],
                                    redirects: vec![],
                                    assignments: vec![],
                                }],
                            }],
                            background: false,
                        }]),
                        redirects: vec![],
                    }],
                }],
                background: false,
            }],
        })
    );

    assert_eq!(
        parser.parse("for arg in hello world; do echo ---------; cowsay $arg; done"),
        Ok(Ast {
            terms: vec![Term {
                code: "for arg in hello world; do echo ---------; cowsay $arg; done".into(),
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::For {
                        var_name: "arg".into(),
                        words: literal_word_vec!["hello", "world"],
                        body: vec![
                            Term {
                                code: "echo ---------".into(),
                                background: false,
                                pipelines: vec![Pipeline {
                                    run_if: RunIf::Always,
                                    commands: vec![Command::SimpleCommand {
                                        external: false,
                                        argv: literal_word_vec!["echo", "---------"],
                                        redirects: vec![],
                                        assignments: vec![],
                                    }],
                                }],
                            },
                            Term {
                                code: "cowsay $arg".into(),
                                background: false,
                                pipelines: vec![Pipeline {
                                    run_if: RunIf::Always,
                                    commands: vec![Command::SimpleCommand {
                                        external: false,
                                        argv: vec![
                                            lit!("cowsay"),
                                            param!("arg", ExpansionOp::GetOrEmpty, false),
                                        ],
                                        redirects: vec![],
                                        assignments: vec![],
                                    }],
                                }],
                            },
                        ],
                    }],
                }],
            }],
        })
    );

    assert_eq!(
        parser.parse(concat!(
            "for arg in hello world; do",
            "   if sometimes-true; then\n",
            "       break\n",
            "   fi\n",
            "   if sometimes-true; then\n",
            "       continue;\n",
            "   fi\n",
            "   something &\n",
            "done"
        )),
        Ok(Ast {
            terms: vec![Term {
                code: "for arg in hello world; do   if sometimes-true; then\n       break\n   fi\n   if sometimes-true; then\n       continue;\n   fi\n   something &\ndone".into(),
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::For {
                        var_name: "arg".into(),
                        words: literal_word_vec!["hello", "world"],
                        body: vec![
                            Term {
                                code: "if sometimes-true; then\n       break\n   fi".into(),
                                background: false,
                                pipelines: vec![Pipeline {
                                    run_if: RunIf::Always,
                                    commands: vec![Command::If {
                                        condition: vec![Term {
                                            code: "sometimes-true".into(),
                                            pipelines: vec![Pipeline {
                                                run_if: RunIf::Always,
                                                commands: vec![Command::SimpleCommand {
                                                    external: false,
                                                    argv: vec![lit!("sometimes-true")],
                                                    redirects: vec![],
                                                    assignments: vec![],
                                                }],
                                            }],
                                            background: false,
                                        }],
                                        then_part: vec![Term {
                                            code: "break".into(),
                                            pipelines: vec![Pipeline {
                                                run_if: RunIf::Always,
                                                commands: vec![Command::Break],
                                            }],
                                            background: false,
                                        }],
                                        elif_parts: vec![],
                                        else_part: None,
                                        redirects: vec![],
                                    }]
                                }]
                            },
                            Term {
                                code: "if sometimes-true; then\n       continue;\n   fi".into(),
                                background: false,
                                pipelines: vec![Pipeline {
                                    run_if: RunIf::Always,
                                    commands: vec![Command::If {
                                        condition: vec![Term {
                                            code: "sometimes-true".into(),
                                            pipelines: vec![Pipeline {
                                                run_if: RunIf::Always,
                                                commands: vec![Command::SimpleCommand {
                                                    external: false,
                                                    argv: vec![lit!("sometimes-true")],
                                                    redirects: vec![],
                                                    assignments: vec![],
                                                }],
                                            }],
                                            background: false,
                                        }],
                                        then_part: vec![Term {
                                            code: "continue".into(),
                                            pipelines: vec![Pipeline {
                                                run_if: RunIf::Always,
                                                commands: vec![Command::Continue],
                                            }],
                                            background: false,
                                        }],
                                        elif_parts: vec![],
                                        else_part: None,
                                        redirects: vec![],
                                    }]
                                }]
                            },
                            Term {
                                code: "something ".into(),
                                background: true,
                                pipelines: vec![Pipeline {
                                        run_if: RunIf::Always,
                                        commands: vec![Command::SimpleCommand {
                                            external: false,
                                            argv: vec![
                                                lit!("something"),
                                            ],
                                            redirects: vec![],
                                            assignments: vec![],
                                        }],
                                    }
                                ]
                            },
                        ],
                    }],
                }],
            }],
        })
    );

    assert_eq!(
        parser.parse("{ echo hello; echo world; }"),
        Ok(Ast {
            terms: vec![Term {
                code: "{ echo hello; echo world; }".into(),
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::Group {
                        terms: vec![
                            Term {
                                code: "echo hello".into(),
                                background: false,
                                pipelines: vec![Pipeline {
                                    run_if: RunIf::Always,
                                    commands: vec![Command::SimpleCommand {
                                        external: false,
                                        argv: literal_word_vec!["echo", "hello"],
                                        redirects: vec![],
                                        assignments: vec![],
                                    }],
                                }],
                            },
                            Term {
                                code: "echo world".into(),
                                background: false,
                                pipelines: vec![Pipeline {
                                    run_if: RunIf::Always,
                                    commands: vec![Command::SimpleCommand {
                                        external: false,
                                        argv: literal_word_vec!["echo", "world"],
                                        redirects: vec![],
                                        assignments: vec![],
                                    }],
                                }],
                            },
                        ],
                    }],
                }],
            }],
        })
    );

    assert_eq!(
        parser.parse("( echo hello; echo world; )"),
        Ok(Ast {
            terms: vec![Term {
                code: "( echo hello; echo world; )".into(),
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::SubShellGroup {
                        terms: vec![
                            Term {
                                code: "echo hello".into(),
                                background: false,
                                pipelines: vec![Pipeline {
                                    run_if: RunIf::Always,
                                    commands: vec![Command::SimpleCommand {
                                        external: false,
                                        argv: literal_word_vec!["echo", "hello"],
                                        redirects: vec![],
                                        assignments: vec![],
                                    }],
                                }],
                            },
                            Term {
                                code: "echo world".into(),
                                background: false,
                                pipelines: vec![Pipeline {
                                    run_if: RunIf::Always,
                                    commands: vec![Command::SimpleCommand {
                                        external: false,
                                        argv: literal_word_vec!["echo", "world"],
                                        redirects: vec![],
                                        assignments: vec![],
                                    }],
                                }],
                            },
                        ],
                    }],
                }],
            }],
        })
    );

    assert_eq!(
        parser.parse(concat!(
            "  case $action in\n",
            "    echo) echo action is echo ;;\n",
            "    date | time) echo action is date; date \n",
            "       ;;\n",
            "    *)\n",
            "       # No need to set ARM_EABI_TOOLCHAIN for other ARCHs\n",
            "       ;;\n",
            "  esac\n"
        )),
        Ok(Ast {
            terms: vec![Term {
                code: "case $action in\n    echo) echo action is echo ;;\n    date | time) echo action is date; date \n       ;;\n    *)\n       # No need to set ARM_EABI_TOOLCHAIN for other ARCHs\n       ;;\n  esac".into(),
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::Case {
                        word: param!("action", ExpansionOp::GetOrEmpty, false),
                        cases: vec![
                            CaseItem {
                                patterns: vec![lit!("echo")],
                                body: vec![Term {
                                    code: "echo action is echo ".into(),
                                    background: false,
                                    pipelines: vec![Pipeline {
                                        run_if: RunIf::Always,
                                        commands: vec![Command::SimpleCommand {
                                            external: false,
                                            argv: literal_word_vec!["echo", "action", "is", "echo"],
                                            redirects: vec![],
                                            assignments: vec![],
                                        }],
                                    }],
                                }],
                            },
                            CaseItem {
                                patterns: vec![lit!("date"), lit!("time")],
                                body: vec![
                                    Term {
                                        code: "echo action is date".into(),
                                        background: false,
                                        pipelines: vec![Pipeline {
                                            run_if: RunIf::Always,
                                            commands: vec![Command::SimpleCommand {
                                                external: false,
                                                argv: literal_word_vec![
                                                    "echo", "action", "is", "date"
                                                ],
                                                redirects: vec![],
                                                assignments: vec![],
                                            }],
                                        }],
                                    },
                                    Term {
                                        code: "date ".into(),
                                        background: false,
                                        pipelines: vec![Pipeline {
                                            run_if: RunIf::Always,
                                            commands: vec![Command::SimpleCommand {
                                                external: false,
                                                argv: literal_word_vec!["date"],
                                                redirects: vec![],
                                                assignments: vec![],
                                            }],
                                        }],
                                    },
                                ],
                            },
                            CaseItem {
                                patterns: vec![lit!("*")],
                                body: vec![]
                            },
                        ],
                    }],
                }],
            }],
        })
    );

    assert_eq!(
        parser.parse("function func1()\n { echo hello; echo world; return 3;\n }; func1"),
        Ok(Ast {
            terms: vec![
                Term {
                    code: "function func1()\n { echo hello; echo world; return 3;\n }".into(),
                    background: false,
                    pipelines: vec![Pipeline {
                        run_if: RunIf::Always,
                        commands: vec![Command::FunctionDef {
                            name: "func1".into(),
                            body: Box::new(Command::Group {
                                terms: vec![
                                    Term {
                                        code: "echo hello".into(),
                                        background: false,
                                        pipelines: vec![Pipeline {
                                            run_if: RunIf::Always,
                                            commands: vec![Command::SimpleCommand {
                                                external: false,
                                                argv: literal_word_vec!["echo", "hello"],
                                                redirects: vec![],
                                                assignments: vec![],
                                            }],
                                        }],
                                    },
                                    Term {
                                        code: "echo world".into(),
                                        background: false,
                                        pipelines: vec![Pipeline {
                                            run_if: RunIf::Always,
                                            commands: vec![Command::SimpleCommand {
                                                external: false,
                                                argv: literal_word_vec!["echo", "world"],
                                                redirects: vec![],
                                                assignments: vec![],
                                            }],
                                        }],
                                    },
                                    Term {
                                        code: "return 3".into(),
                                        background: false,
                                        pipelines: vec![Pipeline {
                                            run_if: RunIf::Always,
                                            commands: vec![Command::Return {
                                                status: Some(Word(vec![Span::Literal(3.to_string())]))
                                            }],
                                        }],
                                    },
                                ],
                            }),
                        }],
                    }],
                },
                Term {
                    code: "func1".into(),
                    background: false,
                    pipelines: vec![Pipeline {
                        run_if: RunIf::Always,
                        commands: vec![Command::SimpleCommand {
                            external: false,
                            argv: vec![lit!("func1")],
                            redirects: vec![],
                            assignments: vec![],
                        }],
                    }],
                },
            ],
        })
    );

    assert_eq!(
        parser.parse("x=$((123)); func2() { local x=456 y z; echo $((x * 2))\n return; }; echo $x"),
        Ok(Ast {
            terms: vec![
                Term {
                    code: "x=$((123))".into(),
                    background: false,
                    pipelines: vec![Pipeline {
                        run_if: RunIf::Always,
                        commands: vec![Command::Assignment {
                            assignments: vec![Assignment {
                                append: false,
                                name: "x".into(),
                                initializer: Initializer::String(Word(vec![Span::ArithExpr {
                                    expr: Box::new(Expr::Literal(123))
                                }])),
                                index: None,
                            }],
                        }],
                    }],
                },
                Term {
                    code: "func2() { local x=456 y z; echo $((x * 2))\n return; }".into(),
                    background: false,
                    pipelines: vec![Pipeline {
                        run_if: RunIf::Always,
                        commands: vec![Command::FunctionDef {
                            name: "func2".into(),
                            body: Box::new(Command::Group {
                                terms: vec![
                                    Term {
                                        code: "local x=456 y z".into(),
                                        background: false,
                                        pipelines: vec![Pipeline {
                                            run_if: RunIf::Always,
                                            commands: vec![Command::LocalDef {
                                                declarations: vec![
                                                    LocalDeclaration::Assignment(Assignment {
                                                        name: "x".into(),
                                                        initializer: Initializer::String(Word(
                                                            vec![Span::Literal("456".into())]
                                                        )),
                                                        index: None,
                                                        append: false,
                                                    }),
                                                    LocalDeclaration::Name("y".into()),
                                                    LocalDeclaration::Name("z".into())
                                                ]
                                            }],
                                        }],
                                    },
                                    Term {
                                        code: "echo $((x * 2))".into(),
                                        background: false,
                                        pipelines: vec![Pipeline {
                                            run_if: RunIf::Always,
                                            commands: vec![Command::SimpleCommand {
                                                external: false,
                                                argv: vec![
                                                    lit!("echo"),
                                                    Word(vec![Span::ArithExpr {
                                                        expr: Box::new(Expr::Mul(BinaryExpr {
                                                            lhs: Box::new(Expr::Parameter {
                                                                name: "x".into()
                                                            }),
                                                            rhs: Box::new(Expr::Literal(2)),
                                                        }))
                                                    }])
                                                ],
                                                redirects: vec![],
                                                assignments: vec![],
                                            }],
                                        }],
                                    },
                                    Term {
                                        code: "return".into(),
                                        background: false,
                                        pipelines: vec![Pipeline {
                                            run_if: RunIf::Always,
                                            commands: vec![Command::Return { status: None }],
                                        }],
                                    },
                                ],
                            }),
                        }],
                    }],
                },
                Term {
                    code: "echo $x".into(),
                    background: false,
                    pipelines: vec![Pipeline {
                        run_if: RunIf::Always,
                        commands: vec![Command::SimpleCommand {
                            external: false,
                            argv: vec![
                                lit!("echo"),
                                param!("x", ExpansionOp::GetOrEmpty, false)
                                // Word(vec![Span::Parameter {
                                //     name: "x".into(),
                                //     op: ExpansionOp::GetOrEmpty,
                                //     quoted: false,
                                // }])
                            ],
                            redirects: vec![],
                            assignments: vec![],
                        }],
                    }],
                },
            ],
        })
    );

    assert_eq!(
        parser.parse("for ((i = 0; i < 4; i++));\ndo echo $i\ndone"),
        Ok(Ast {
            terms: vec![Term {
                code: "for ((i = 0; i < 4; i++));\ndo echo $i\ndone".into(),
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::ArithFor {
                        init: Box::new(Expr::Assign {
                            name: "i".to_owned(),
                            rhs: Box::new(Expr::Literal(0))
                        }),
                        cond: Box::new(Expr::Lt(
                            Box::new(Expr::Parameter { name: "i".into() }),
                            Box::new(Expr::Literal(4))
                        )),
                        update: Box::new(Expr::Inc("i".into())),
                        body: vec![Term {
                            code: "echo $i".into(),
                            background: false,
                            pipelines: vec![Pipeline {
                                run_if: RunIf::Always,
                                commands: vec![Command::SimpleCommand {
                                    external: false,
                                    argv: vec![
                                        Word(vec![Span::Literal("echo".into())]),
                                        param!("i", ExpansionOp::GetOrEmpty, false),
                                    ],
                                    redirects: vec![],
                                    assignments: vec![],
                                }]
                            }],
                        }],
                    }],
                }],
            }],
        })
    );
}

#[test]
pub fn test_redirections() {
    let parser = ShellParser::new();

    assert_eq!(
        parser.parse("ls &> /dev/null\n"),
        Ok(Ast {
            terms: vec![Term {
                code: "ls &> /dev/null".into(),
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::SimpleCommand {
                        external: false,
                        argv: literal_word_vec!["ls"],
                        redirects: vec![
                            Redirection {
                                fd: 1,
                                direction:  RedirectionDirection::Output,
                                target: RedirectionType::FileOrFd(Word(vec![Span::Literal("/dev/null".to_string())]))
                            },
                            Redirection {
                                fd: 2,
                                direction: RedirectionDirection::Output,
                                target: RedirectionType::Fd(1)
                            }
                        ],
                        assignments: vec![],
                    }],
                }],
            }],
        })
    );

}

#[test]
pub fn test_expansions() {
    let parser = ShellParser::new();

    assert_eq!(
        parser.parse("ls `echo   -l`"),
        Ok(Ast {
            terms: vec![Term {
                code: "ls `echo   -l`".into(),
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::SimpleCommand {
                        external: false,
                        argv: vec![
                            Word(vec![Span::Literal("ls".into())]),
                            Word(vec![Span::Command {
                                quoted: false,
                                body: vec![Term {
                                    code: "echo   -l".into(),
                                    background: false,
                                    pipelines: vec![Pipeline {
                                        run_if: RunIf::Always,
                                        commands: vec![Command::SimpleCommand {
                                            external: false,
                                            argv: vec![
                                                Word(vec![Span::Literal("echo".into())]),
                                                Word(vec![Span::Literal("-l".into())]),
                                            ],
                                            redirects: vec![],
                                            assignments: vec![],
                                        }],
                                    }],
                                }],
                            }]),
                        ],
                        redirects: vec![],
                        assignments: vec![],
                    }],
                }],
            }],
        })
    );

    assert_eq!(
        parser.parse("ls $(echo -l)"),
        Ok(Ast {
            terms: vec![Term {
                code: "ls $(echo -l)".into(),
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::SimpleCommand {
                        external: false,
                        argv: vec![
                            Word(vec![Span::Literal("ls".into())]),
                            Word(vec![Span::Command {
                                quoted: false,
                                body: vec![Term {
                                    code: "echo -l".into(),
                                    background: false,
                                    pipelines: vec![Pipeline {
                                        run_if: RunIf::Always,
                                        commands: vec![Command::SimpleCommand {
                                            external: false,
                                            argv: vec![
                                                Word(vec![Span::Literal("echo".into())]),
                                                Word(vec![Span::Literal("-l".into())]),
                                            ],
                                            redirects: vec![],
                                            assignments: vec![],
                                        }],
                                    }],
                                }],
                            }]),
                        ],
                        redirects: vec![],
                        assignments: vec![],
                    }],
                }],
            }],
        })
    );

    assert_eq!(
        parser.parse("echo \"$TERM\""),
        Ok(Ast {
            terms: vec![Term {
                code: "echo \"$TERM\"".into(),
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::SimpleCommand {
                        external: false,
                        argv: vec![
                            Word(vec![Span::Literal("echo".into())]),
                            param!("TERM", ExpansionOp::GetOrEmpty, true),
                        ],
                        redirects: vec![],
                        assignments: vec![],
                    }],
                }],
            }],
        })
    );

    assert_eq!(
        parser.parse("echo $? $7"),
        Ok(Ast {
            terms: vec![Term {
                code: "echo $? $7".into(),
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::SimpleCommand {
                        external: false,
                        argv: vec![
                            Word(vec![Span::Literal("echo".into())]),
                            param!("?", ExpansionOp::GetOrEmpty, false),
                            param!("7", ExpansionOp::GetOrEmpty, false),
                        ],
                        redirects: vec![],
                        assignments: vec![],
                    }],
                }],
            }],
        })
    );

    assert_eq!(
        parser.parse("foo ${var1:-a${xyz}b} bar"),
        Ok(Ast {
            terms: vec![Term {
                code: "foo ${var1:-a${xyz}b} bar".into(),
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::SimpleCommand {
                        external: false,
                        argv: vec![
                            lit!("foo"),
                            param!("var1",
                                ExpansionOp::GetOrAction(":-".into(), Word(vec![
                                    Span::Literal("a".into()),
                                    Span::Parameter {
                                        name: Box::new(Span::Literal("xyz".into())),
                                        index: None,
                                        op: ExpansionOp::GetOrEmpty,
                                        quoted: false,
                                    },
                                    Span::Literal("b".into()),
                                ])),
                                false),
                            lit!("bar"),
                        ],
                        redirects: vec![],
                        assignments: vec![],
                    }],
                }],
            }],
        })
    );

    assert_eq!(
        parser.parse(r#"echo ${undefined:-Current} "\"\$"${undefined:=TERM}\" "is $TERM len=${#TERM}""#),
        Ok(Ast {
            terms: vec![Term {
                code: r#"echo ${undefined:-Current} "\"\$"${undefined:=TERM}\" "is $TERM len=${#TERM}""#.into(),
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::SimpleCommand {
                        external: false,
                        argv: vec![
                            Word(vec![Span::Literal("echo".into())]),
                            Word(vec![Span::Parameter {
                                name: Box::new(Span::Literal("undefined".into())),
                                op: ExpansionOp::GetOrAction(":-".into(), Word(vec![Span::Literal(
                                    "Current".into(),
                                )])),
                                index: None,
                                quoted: false,
                            }]),
                            Word(vec![
                                Span::Literal("\"$".to_owned()),
                                Span::Parameter {
                                    name: Box::new(Span::Literal("undefined".into())),
                                    op: ExpansionOp::GetOrAction(":=".into(), Word(vec![Span::Literal(
                                        "TERM".into(),
                                    )])),
                                    index: None,
                                    quoted: false,
                                },
                                Span::Literal("\"".to_owned()),
                            ]),
                            Word(vec![
                                Span::Literal("is ".into()),
                                Span::Parameter {
                                    name: Box::new(Span::Literal("TERM".into())),
                                    op: ExpansionOp::GetOrEmpty,
                                    index: None,
                                    quoted: true,
                                },
                                Span::Literal(" len=".into()),
                                Span::Parameter {
                                    name: Box::new(Span::Literal("TERM".into())),
                                    op: ExpansionOp::Length,
                                    index: None,
                                    quoted: true,
                                },
                            ]),
                        ],
                        redirects: vec![],
                        assignments: vec![],
                    }],
                }],
            }],
        })
    );

    assert_eq!(
        parser.parse("echo ${var/a*\\/e/12345}"),
        Ok(Ast {
            terms: vec![Term {
                code: "echo ${var/a*\\/e/12345}".into(),
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::SimpleCommand {
                        external: false,
                        argv: vec![
                            lit!("echo"),
                            Word(vec![Span::Parameter {
                                name: Box::new(Span::Literal("var".into())),
                                index: None,
                                quoted: false,
                                op: ExpansionOp::Subst {
                                    pattern: "a*/e".into(),
                                    replacement: "12345".into(),
                                    op: None,
                                }
                            }]),
                        ],
                        redirects: vec![],
                        assignments: vec![],
                    }],
                }],
            }],
        })
    );
}

#[test]
pub fn test_append() {
    let parser = ShellParser::new();

    assert_eq!(
        parser.parse("a+=abc"),
        Ok(Ast {
            terms: vec![Term {
                code: "a+=abc".into(),
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::Assignment {
                        assignments: vec![Assignment {
                            name: "a".into(),
                            initializer: Initializer::String(Word(vec![Span::Literal("abc".into())])),
                            index: None,
                            append: true,
                        }],
                    }],
                }],
            }],
        })
    );

    assert_eq!(
        parser.parse("a+=( k i n g )"),
        Ok(Ast {
            terms: vec![Term {
                code: "a+=( k i n g )".into(),
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::Assignment {
                        assignments: vec![Assignment {
                            name: "a".into(),
                            initializer: Initializer::Array(
                                vec![Word(vec![Span::Literal("k".into())]),
                                     Word(vec![Span::Literal("i".into())]),
                                     Word(vec![Span::Literal("n".into())]),
                                     Word(vec![Span::Literal("g".into())])]),
                            index: None,
                            append: true,
                        }],
                    }],
                }],
            }],
        })
    );
}

#[test]
pub fn test_assignments() {
    let parser = ShellParser::new();

    assert_eq!(
        parser.parse("foo=bar"),
        Ok(Ast {
            terms: vec![Term {
                code: "foo=bar".into(),
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::Assignment {
                        assignments: vec![Assignment {
                            name: "foo".into(),
                            initializer: Initializer::String(Word(vec![Span::Literal(
                                "bar".into()
                            )])),
                            index: None,
                            append: false,
                        }],
                    }],
                }],
            }],
        })
    );

    assert_eq!(
        parser.parse("foo=('I wanna quit gym' 'holy moly' egg 'spam spam beans spam')"),
        Ok(Ast {
            terms: vec![Term {
                code: "foo=('I wanna quit gym' 'holy moly' egg 'spam spam beans spam')".into(),
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::Assignment {
                        assignments: vec![Assignment {
                            name: "foo".into(),
                            initializer: Initializer::Array(vec![
                                Word(vec![Span::Literal("I wanna quit gym".into())]),
                                Word(vec![Span::Literal("holy moly".into())]),
                                Word(vec![Span::Literal("egg".into())]),
                                Word(vec![Span::Literal("spam spam beans spam".into())]),
                            ]),
                            index: None,
                            append: false,
                        }],
                    }],
                }],
            }],
        })
    );

    assert_eq!(
        parser.parse("foo[k + 7 * c]=bar"),
        Ok(Ast {
            terms: vec![Term {
                code: "foo[k + 7 * c]=bar".into(),
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::Assignment {
                        assignments: vec![Assignment {
                            name: "foo".into(),
                            initializer: Initializer::String(Word(vec![Span::Literal(
                                "bar".into()
                            )])),
                            index: Some(Box::new(Expr::Add(BinaryExpr {
                                lhs: Box::new(Expr::Parameter { name: "k".into() }),
                                rhs: Box::new(Expr::Mul(BinaryExpr {
                                    lhs: Box::new(Expr::Literal(7)),
                                    rhs: Box::new(Expr::Parameter { name: "c".into() }),
                                }))
                            }))),
                            append: false,
                        }],
                    }],
                }],
            }],
        })
    );

    assert_eq!(
        parser.parse("nobody=expects the=\"spanish inquisition\""),
        Ok(Ast {
            terms: vec![Term {
                code: "nobody=expects the=\"spanish inquisition\"".into(),
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::Assignment {
                        assignments: vec![
                            Assignment {
                                name: "nobody".into(),
                                initializer: Initializer::String(Word(vec![Span::Literal(
                                    "expects".into()
                                )])),
                                index: None,
                                append: false,
                            },
                            Assignment {
                                name: "the".into(),
                                initializer: Initializer::String(Word(vec![Span::Literal(
                                    "spanish inquisition".into()
                                )])),
                                index: None,
                                append: false,
                            },
                        ],
                    }],
                }],
            }],
        })
    );
}

#[test]
pub fn test_tilde() {
    let parser = ShellParser::new();

    assert_eq!(
        parser.parse("echo ~ ~/usr ~seiya ~seiya/usr a/~/b"),
        Ok(Ast {
            terms: vec![Term {
                code: "echo ~ ~/usr ~seiya ~seiya/usr a/~/b".into(),
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::SimpleCommand {
                        external: false,
                        argv: vec![
                            Word(vec![Span::Literal("echo".into())]),
                            Word(vec![Span::Tilde(None)]),
                            Word(vec![Span::Tilde(None), Span::Literal("/usr".into())]),
                            Word(vec![Span::Tilde(Some("seiya".into()))]),
                            Word(vec![
                                Span::Tilde(Some("seiya".into())),
                                Span::Literal("/usr".into()),
                            ]),
                            Word(vec![Span::Literal("a/~/b".into())]),
                        ],
                        redirects: vec![],
                        assignments: vec![],
                    }],
                }],
            }],
        })
    );
}

#[test]
pub fn test_assign_like_prefix() {
    let parser = ShellParser::new();

    assert_eq!(
        parser.parse("./configure --prefix=~/usr in\\valid=~"),
        Ok(Ast {
            terms: vec![Term {
                code: "./configure --prefix=~/usr in\\valid=~".into(),
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::SimpleCommand {
                        external: false,
                        argv: vec![
                            Word(vec![Span::Literal("./configure".into())]),
                            Word(vec![
                                Span::Literal("--prefix=".into()), Span::Tilde(None), Span::Literal("/usr".into()),
                            ]),
                            Word(vec![
                                Span::Literal("invalid=".into()), Span::Tilde(None),
                            ]),
                        ],
                        redirects: vec![],
                        assignments: vec![],
                    }],
                }],
            }],
        })
    );
}

#[test]
pub fn test_arith_expr() {
    let parser = ShellParser::new();

    assert_eq!(
        parser.parse("echo $(( 1 + 2+(-3) ))"),
        Ok(Ast {
            terms: vec![Term {
                code: "echo $(( 1 + 2+(-3) ))".into(),
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::SimpleCommand {
                        external: false,
                        argv: vec![
                            Word(vec![Span::Literal("echo".into())]),
                            Word(vec![Span::ArithExpr {
                                expr: Box::new(Expr::Add(BinaryExpr {
                                    lhs: Box::new(Expr::Literal(1)),
                                    rhs: Box::new(Expr::Add(BinaryExpr {
                                        lhs: Box::new(Expr::Literal(2)),
                                        rhs: Box::new(Expr::Minus(Box::new(Expr::Literal(3)))),
                                    })),
                                })),
                            }]),
                        ],
                        redirects: vec![],
                        assignments: vec![],
                    }],
                }],
            }],
        })
    );

    assert_eq!(
        parser.parse("echo $((1+2*$foo-bar))"),
        Ok(Ast {
            terms: vec![Term {
                code: "echo $((1+2*$foo-bar))".into(),
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::SimpleCommand {
                        external: false,
                        argv: vec![
                            Word(vec![Span::Literal("echo".into())]),
                            Word(vec![Span::ArithExpr {
                                expr: Box::new(Expr::Add(BinaryExpr {
                                    lhs: Box::new(Expr::Literal(1)),
                                    rhs: Box::new(Expr::Sub(BinaryExpr {
                                        lhs: Box::new(Expr::Mul(BinaryExpr {
                                            lhs: Box::new(Expr::Literal(2)),
                                            rhs: Box::new(Expr::Parameter { name: "foo".into() }),
                                        })),
                                        rhs: Box::new(Expr::Parameter { name: "bar".into() }),
                                    })),
                                })),
                            }]),
                        ],
                        redirects: vec![],
                        assignments: vec![],
                    }],
                }],
            }],
        })
    );

    assert_eq!(
        parser.parse("$((($tdiff % 3600) / 60))"),
        Ok(Ast {
            terms: vec![Term {
                code: "$((($tdiff % 3600) / 60))".into(),
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::SimpleCommand {
                        external: false,
                        argv: vec![
                            Word(vec![Span::ArithExpr {
                                expr: Box::new(Expr::Div(BinaryExpr {
                                    lhs: Box::new(Expr::Modulo(BinaryExpr {
                                        lhs: Box::new(Expr::Parameter { name: "tdiff".into() }),
                                        rhs: Box::new(Expr::Literal(3600))
                                    })),
                                    rhs: Box::new(Expr::Literal(60))
                                })) ,
                            }]),
                        ],
                        redirects: vec![],
                        assignments: vec![]
                    }],
                }],
            }],
        }),
    );
}

#[test]
pub fn test_patterns() {
    let parser = ShellParser::new();

    assert_eq!(
        parser.parse("echo * a?c"),
        Ok(Ast {
            terms: vec![Term {
                code: "echo * a?c".into(),
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::SimpleCommand {
                        external: false,
                        argv: vec![
                            Word(vec![Span::Literal("echo".into())]),
                            Word(vec![Span::Literal("*".into())]),
                            Word(vec![Span::Literal("a?c".into())]),
                        ],
                        redirects: vec![],
                        assignments: vec![],
                    }],
                }],
            }],
        })
    );
}

#[test]
pub fn test_comments() {
    let parser = ShellParser::new();

    assert_eq!(
        parser.parse("foo bar # this is comment\n#comment line\nls -G /tmp # hello world\n"),
        Ok(Ast {
            terms: vec![
                Term {
                    code: "foo bar ".into(),
                    background: false,
                    pipelines: vec![Pipeline {
                        run_if: RunIf::Always,
                        commands: vec![Command::SimpleCommand {
                            external: false,
                            argv: literal_word_vec!["foo", "bar"],
                            redirects: vec![],
                            assignments: vec![],
                        }],
                    }],
                },
                Term {
                    code: "ls -G /tmp ".into(),
                    background: false,
                    pipelines: vec![Pipeline {
                        run_if: RunIf::Always,
                        commands: vec![Command::SimpleCommand {
                            external: false,
                            argv: literal_word_vec!["ls", "-G", "/tmp"],
                            redirects: vec![],
                            assignments: vec![],
                        }],
                    }],
                },
            ],
        })
    );

    assert_eq!(parser.parse("# Hello\n"), Err(ParseError::Empty));
    assert_eq!(parser.parse("# Hello\n#World"), Err(ParseError::Empty));
}

#[test]
pub fn test_string_literal() {
    let parser = ShellParser::new();

    assert_eq!(
        parser.parse("docker run --env PS1=\"DAC(\\#)[\\d \\T:\\w]\\\\$ \""),
        Ok(Ast {
            terms: vec![Term {
                code: "docker run --env PS1=\"DAC(\\#)[\\d \\T:\\w]\\\\$ \"".into(),
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::SimpleCommand {
                        external: false,
                        argv: vec![
                            lit!("docker"),
                            lit!("run"),
                            lit!("--env"),
                            Word(vec![
                                Span::Literal("PS1=".into()),
                                Span::Literal("DAC(\\#)[\\d \\T:\\w]\\".into()),
                                Span::Literal("$ ".into())]),
                        ],
                        redirects: vec![],
                        assignments: vec![]
                    }]
                }],
            }],
        })
    );

    assert_eq!(
        parser.parse("echo \"hello\""),
        Ok(Ast {
            terms: vec![Term {
                code: "echo \"hello\"".into(),
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::SimpleCommand {
                        external: false,
                        argv: vec![lit!("echo"), lit!("hello")],
                        assignments: vec![],
                        redirects: vec![],
                    }]
                }]
            }]
        })
    );

    assert_eq!(
        parser.parse("echo -e \"\" \"hello\\tworld\""),
        Ok(Ast {
            terms: vec![Term {
                code: "echo -e \"\" \"hello\\tworld\"".into(),
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::SimpleCommand {
                        external: false,
                        argv: vec![
                            lit!("echo"),
                            lit!("-e"),
                            Word(vec![]),
                            lit!("hello\\tworld")
                        ],
                        assignments: vec![],
                        redirects: vec![],
                    }]
                }]
            }]
        })
    );

    assert_eq!(
        parser.parse("echo abc\"de\"fg \"1'2''3\""),
        Ok(Ast {
            terms: vec![Term {
                code: "echo abc\"de\"fg \"1'2''3\"".into(),
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::SimpleCommand {
                        external: false,
                        argv: vec![
                            lit!("echo"),
                            Word(vec![
                                Span::Literal("abc".into()),
                                Span::Literal("de".into()),
                                Span::Literal("fg".into()),
                            ]),
                            lit!("1'2''3")
                        ],
                        assignments: vec![],
                        redirects: vec![],
                    }]
                }]
            }]
        })
    );
}

#[test]
pub fn test_escape_sequences() {
    let parser = ShellParser::new();

    assert_eq!(
        parser.parse(r#"echo "\e[1m" \$a"b;\n\"c"d \\n \this_\i\s_\normal"#),
        Ok(Ast {
            terms: vec![Term {
                code: "echo \"\\e[1m\" \\$a\"b;\\n\\\"c\"d \\\\n \\this_\\i\\s_\\normal".into(),
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::SimpleCommand {
                        external: false,
                        argv: vec![
                            lit!("echo"),
                            lit!("\\e[1m"),
                            Word(vec![
                                Span::Literal("$a".into()),
                                Span::Literal("b;\\n\"c".into()),
                                Span::Literal("d".into()),
                            ]),
                            lit!("\\n"),
                            lit!("this_is_normal")
                        ],
                        assignments: vec![],
                        redirects: vec![],
                    }]
                }]
            }]
        })
    );
}

#[test]
pub fn test_courner_cases() {
    let parser = ShellParser::new();

    assert_eq!(parser.parse(""), Err(ParseError::Empty));
    assert_eq!(parser.parse("\n"), Err(ParseError::Empty));
    assert_eq!(parser.parse("\n\n\n"), Err(ParseError::Empty));
    assert_eq!(parser.parse("\n\t\n"), Err(ParseError::Empty));
    assert_eq!(parser.parse("  "), Err(ParseError::Empty));
    assert!(parser.parse(";;;;;;").is_err());
    assert!(parser.parse("||").is_err());
    assert!(parser.parse("& &&").is_err());
}

#[test]
pub fn test_process_substitution() {
    let parser = ShellParser::new();

    assert_eq!(
        parser.parse("cat <(echo hello from a file)"),
        Ok(Ast {
            terms: vec![Term {
                code: "cat <(echo hello from a file)".into(),
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::SimpleCommand {
                        external: false,
                        argv: vec![
                            Word(vec![Span::Literal("cat".into())]),
                            Word(vec![Span::ProcSubst {
                                subst_type: ProcSubstType::StdoutToFile,
                                body: vec![Term {
                                    code: "echo hello from a file".into(),
                                    background: false,
                                    pipelines: vec![Pipeline {
                                        run_if: RunIf::Always,
                                        commands: vec![Command::SimpleCommand {
                                            external: false,
                                            argv: vec![
                                                Word(vec![Span::Literal("echo".into())]),
                                                Word(vec![Span::Literal("hello".into())]),
                                                Word(vec![Span::Literal("from".into())]),
                                                Word(vec![Span::Literal("a".into())]),
                                                Word(vec![Span::Literal("file".into())]),
                                            ],
                                            redirects: vec![],
                                            assignments: vec![],
                                        }],
                                    }],
                                }],
                            }]),
                        ],
                        redirects: vec![],
                        assignments: vec![],
                    }],
                }],
            }],
        })
    );
}

#[test]
pub fn test_cond_ex() {
    let parser = ShellParser::new();

    assert_eq!(
        parser.parse("hello=world; [[ $hello == world ]]"),
        Ok(Ast {
            terms: vec![
                Term {
                    code: "hello=world".into(),
                    background: false,
                    pipelines: vec![Pipeline {
                        run_if: RunIf::Always,
                        commands: vec![Command::Assignment {
                            assignments: vec![Assignment {
                                name: "hello".into(),
                                initializer: Initializer::String(lit!("world")),
                                index: None,
                                append: false,
                            }],
                        }],
                    }],
                },
                Term {
                    code: "[[ $hello == world ]]".into(),
                    background: false,
                    pipelines: vec![Pipeline {
                        run_if: RunIf::Always,
                        commands: vec![Command::Cond{is_not: false, expr: Some(Box::new(CondExpr::StrEq(
                            Box::new(CondExpr::Word(param!("hello", ExpansionOp::GetOrEmpty, false))),
                            Box::new(CondExpr::Word(lit!("world"),))
                        )))}]
                    }],
                },
            ]
        })
    );
}

#[test]
pub fn test_export_empty() {
    let parser = ShellParser::new();

    assert_eq!(
        parser.parse("export PATH="),
        Ok(Ast {
            terms: vec![Term {
                code: "export PATH=".into(),
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::SimpleCommand {
                        external: false,
                        argv: vec![lit!("export"), lit!("PATH=")],
                        redirects: vec![],
                        assignments: vec![],
                    }],
                }],
            }],
        })
    );
}

#[test]
pub fn test_grep() {
    let parser = ShellParser::new();

    assert_eq!(
        parser.parse("grep -q -e \"^[0-9][0-9]*$\""),
        Ok(Ast {
            terms: vec![Term {
                code: "grep -q -e \"^[0-9][0-9]*$\"".into(),
                background: false ,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::SimpleCommand {
                        external: false,
                        argv: vec![lit!("grep"), lit!("-q"), lit!("-e"),
                            Word(vec![Span::Literal("^[0-9][0-9]*".into()),
                                Span::Literal("$".into())])
                        ],
                        redirects: vec![],
                        assignments: vec![]
                    }]
                }],
            }],
        })
    );
}

#[test]
pub fn test_cond_test() {
    let parser = ShellParser::new();

    assert_eq!(
        parser.parse("[[ -z \"$1\" ]]"),
        Ok(Ast {
            terms: vec![Term {
                code: "[[ -z \"$1\" ]]".into(),
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::Cond{is_not: false, expr: Some(Box::new(CondExpr::StrEq(
                        Box::new(CondExpr::Word(param!("1", ExpansionOp::GetOrEmpty, true))),
                        Box::new(CondExpr::Word(lit!(""))))))
                    }],
                }],
            }],
        }),
    );

    assert_eq!(
        parser.parse("[ ! -f $FILELIST ]"),
        Ok(Ast {
            terms: vec![Term {
                code: "[ ! -f $FILELIST ]".into(),
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::SimpleCommand {
                        external: false,
                        argv: vec![lit!("["), lit!("!"), lit!("-f"), param!("FILELIST", ExpansionOp::GetOrEmpty, false), lit!("]")],
                        redirects: vec![],
                        assignments: vec![]
                    }],
                }],
            }],
        }),
    );
}

#[test]
pub fn test_regex() {
    let parser = ShellParser::new();

    assert_eq!(
        parser.parse("[[ $1 =~ ^[0-9]+$ ]]"),
        Ok(Ast {
            terms: vec![Term {
                code: "[[ $1 =~ ^[0-9]+$ ]]".into(),
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::Cond{is_not: false,
                        expr: Some(Box::new(CondExpr::Regex(
                            Box::new(CondExpr::Word(
                                param!("1", ExpansionOp::GetOrEmpty, false))
                            ),
                            "^[0-9]+$".into())
                        ))
                    }],
                }],
            }],
        })
    );
}

#[test]
pub fn test_brace_literal() {
    let parser = ShellParser::new();

    assert_eq!(
        parser.parse("echo src/*.{rs,pest}"),
        Ok(Ast {
            terms: vec![Term {
                code: "echo src/*.{rs,pest}".into(),
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::SimpleCommand {
                        external: false,
                        argv: vec![lit!("echo"),
                            Word(vec![
                                Span::Literal("src/*.".into()),
                                Span::Literal("{rs,pest}".into())],
                                )],
                        redirects: vec![],
                        assignments: vec![],
                    }],
                }],
            }],
        }),
    );
}

#[test]
pub fn test_external_command() {
    let parser = ShellParser::new();

    assert_eq!(
        parser.parse("\\ls"),
        Ok(Ast {
            terms: vec![Term {
                code: "\\ls".into(),
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::SimpleCommand {
                        external: true,
                        argv: vec![lit!("ls")],
                        redirects: vec![],
                        assignments: vec![],
                    }],
                }],
            }],
        })
    );
}

#[test]
pub fn test_heredoc() {
    let parser = ShellParser::new();

    assert_eq!(
        parser.parse(concat!(
            "cat << EOF > file.txt\n",
            "hello world\n",
            "from\n",
            "heredoc!\n",
            "EOF\n"
        )),
        Ok(Ast {
            terms: vec![Term {
                // code: "cat << EOF > file.txt".into(),
                code: "cat << EOF > file.txt\nhello world\nfrom\nheredoc!\nEOF".into(),
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::SimpleCommand {
                        external: false,
                        argv: vec![lit!("cat")],
                        redirects: vec![
                            Redirection {
                                fd: 0,
                                direction: RedirectionDirection::Input,
                                target: RedirectionType::HereDoc(HereDoc(
                                    vec![lit!("hello world"), lit!("from"), lit!("heredoc!")],
                                )),
                            },
                            Redirection {
                                fd: 1,
                                direction: RedirectionDirection::Output,
                                target: RedirectionType::File(lit!("file.txt")),
                            }
                        ],
                        assignments: vec![],
                    }]
                }],
            },]
        })
    );

    assert_ne!(
        parser.parse(concat!(
            "cat << EOF > file.txt\n",
        )),
        // Err(ParseError::Expected {})
        Ok(Ast { terms: vec![] })
    );

    assert_eq!(
        parser.parse(concat!(
            "function hmm() {\n",
            "cat <<FUNC\n",
            "lunch <product_name>-<build_variant>\n",
            "FUNC\n",
            "}\n",
        )),
        Ok(Ast {
            terms: vec![Term {
                code: "function hmm() {\ncat <<FUNC\nlunch <product_name>-<build_variant>\nFUNC\n}".into(),
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::FunctionDef {
                        name: "hmm".into(),
                        body: Box::new(Command::Group { terms: vec![Term {
                            code: "cat <<FUNC\nlunch <product_name>-<build_variant>\nFUNC".into(),
                            background: false,
                            pipelines: vec![Pipeline {
                                run_if: RunIf::Always,
                                commands: vec![Command::SimpleCommand {
                                    external: false,
                                    argv: vec![lit!("cat")],
                                    redirects: vec![Redirection {
                                        fd: 0,
                                        direction: RedirectionDirection::Input,
                                        target: RedirectionType::HereDoc(HereDoc(
                                            vec![lit!("lunch <product_name>-<build_variant>")]
                                        ))
                                    }],
                                    assignments: vec![],
                                }]
                            }]
                        }]})
                    }]
                }]
            }],
        })
    );

    assert_eq!(
        parser.parse(concat!(
            "cat <<EOF\n",
            "lunch $PWD <${product_name}>-<${build_variant}>\n",
            "EOF\n",
        )),
        Ok(Ast {
            terms: vec![Term {
                code: "cat <<EOF\nlunch $PWD <${product_name}>-<${build_variant}>\nEOF".into(),
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::SimpleCommand {
                        external: false,
                        argv: vec![lit!("cat")],
                        redirects: vec![
                            Redirection {
                                fd: 0,
                                direction: RedirectionDirection::Input,
                                target: RedirectionType::HereDoc(HereDoc(
                                    vec![Word(vec![
                                        Span::Literal("lunch ".to_string()),
                                        Span::Parameter {
                                            name: Box::new(Span::Literal("PWD".into())),
                                            op: ExpansionOp::GetOrEmpty,
                                            index: None,
                                            quoted: false
                                        },
                                        Span::Literal(" <".to_string()),
                                        Span::Parameter {
                                            name: Box::new(Span::Literal("product_name".into())),
                                            op: ExpansionOp::GetOrEmpty,
                                            index: None,
                                            quoted: false
                                        },
                                        Span::Literal(">-<".to_string()),
                                        Span::Parameter {
                                            name: Box::new(Span::Literal("build_variant".into())),
                                            op: ExpansionOp::GetOrEmpty,
                                            index: None,
                                            quoted: false
                                        },
                                        Span::Literal(">".to_string())
                                        ])
                                    ])),
                            }
                        ],
                        assignments: vec![],
                    }],
                }],
            }],
        })
    );

    assert_eq!(
        parser.parse(concat!(
            "cat <<- EOF > file.txt\n",
            "\thello world\n",
            "\tfrom\n",
            "\theredoc!\n",
            "EOF\n"
        )),
        Ok(Ast {
            terms: vec![Term {
                code: "cat <<- EOF > file.txt\n\thello world\n\tfrom\n\theredoc!\nEOF".into(),
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::SimpleCommand {
                        external: false,
                        argv: vec![lit!("cat")],
                        redirects: vec![
                            Redirection {
                                fd: 0,
                                direction: RedirectionDirection::Input,
                                target: RedirectionType::HereDoc(HereDoc(
                                    vec![lit!("hello world"), lit!("from"), lit!("heredoc!")],
                                )),
                            },
                            Redirection {
                                fd: 1,
                                direction: RedirectionDirection::Output,
                                target: RedirectionType::File(lit!("file.txt")),
                            }
                        ],
                        assignments: vec![],
                    }]
                }],
            },]
        })
    );

    assert_eq!(
        parser.parse(concat!(
            "cat <<\"EOF\"\n",
            "lunch $PWD <${product_name}>-<${build_variant}>\n",
            "EOF\n",
        )),
        Ok(Ast {
            terms: vec![Term {
                code: "cat <<\"EOF\"\nlunch $PWD <${product_name}>-<${build_variant}>\nEOF".into(),
                background: false,
                pipelines: vec![Pipeline {
                    run_if: RunIf::Always,
                    commands: vec![Command::SimpleCommand {
                        external: false,
                        argv: vec![lit!("cat")],
                        redirects: vec![
                            Redirection {
                                fd: 0,
                                direction: RedirectionDirection::Input,
                                target: RedirectionType::HereDoc(HereDoc(
                                    vec![Word(vec![
                                        Span::Literal("lunch $PWD <${product_name}>-<${build_variant}>".to_string()),
                                        ]),
                                    ])),
                            }
                        ],
                        assignments: vec![],
                    }],
                }],
            }],
        })
    );
}

// #[cfg(test)]
// mod benchmarks {
//     use super::*;
//     use test::Bencher;

//     #[bench]
//     fn newline_parsing_bench(b: &mut Bencher) {
//         b.iter(|| parser.parse("\n"));
//     }

//     #[bench]
//     fn simple_oneliner_parsing_bench(b: &mut Bencher) {
//         b.iter(|| parser.parse("RAILS_ENV=development rails server -p 10022 && echo exited"));
//     }

//     #[bench]
//     fn complex_oneliner_parsing_bench(b: &mut Bencher) {
//         b.iter(|| {
//             parser.parse("func1() { while read line; do echo \"read $line from the prompt!\"; done }")
//         });
//     }
// }
