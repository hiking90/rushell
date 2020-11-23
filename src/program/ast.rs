use std::os::unix::io::RawFd;

#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    InvalidRedirectionTarget,
    InvalidConditionalOperator,
    // Not possible
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Program {
    pub terms: Vec<Term>, // Separated by `&', or `;'.
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Term {
    pub pipelines: Vec<Pipeline>, // Separated by `&&' or `||'.
    pub background: bool,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Pipeline {
    pub run_if: RunIf,
    pub commands: Vec<Command>, // Separated by `|'.
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
pub enum RedirectionDirection {
    Input,  // cat < foo.txt or here document
    Output, // cat > foo.txt
    OutputClobber, // cat >& foo.txt
    InputOutput,
    Append, // cat >> foo.txt
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum RedirectionType {
    File(Word),
    Fd(RawFd),
    Close(RawFd),
    HereDoc(HereDoc),
    /// Since the contents of a here document comes after the current
    /// elemenet (e.g., Command::SimpleCommand), the parser memorizes the
    /// index of here document in `UnresolvedHereDoc` and replace this
    /// with `HereDoc` later. Used internally by the parser.
    UnresolvedHereDoc(usize),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Redirection {
    pub fd: RawFd,
    pub direction: RedirectionDirection,
    pub target: RedirectionType,
}

impl Redirection {
    pub fn new_for_parser(fd: Option<char>, direction: RedirectionDirection, target: RedirectionType) -> Redirection {
        Redirection {
            fd: if let Some(fd) = fd {
                    (fd.to_digit(10).unwrap() - '0'.to_digit(10).unwrap()) as i32
                } else {
                    if direction == RedirectionDirection::Input ||
                       direction == RedirectionDirection::InputOutput {
                        0
                    } else {
                        1
                    }
                },
            direction: direction,
            target: target,
        }
    }
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
    pub index: Option<Expr>,
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
        init: Expr,
        cond: Expr,
        update: Expr,
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
pub enum ExpansionOp {
    Length,                              // ${#parameter}
    GetOrEmpty,                          // $parameter and ${parameter}
    GetOrDefault(Word),                  // ${parameter:-word}
    GetNullableOrDefault(Word),          // ${parameter-word}
    GetOrDefaultAndAssign(Word),         // ${parameter:=word}
    GetNullableOrDefaultAndAssign(Word), // ${parameter=word}
    // ${parameter/pattern/replacement}
    Subst {
        pattern: Word,
        replacement: Word,
        replace_all: bool,
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

    Expr(Box<Expr>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ProcSubstType {
    // <(echo hello)
    StdoutToFile,
    // >(grep hello)
    FileToStdin,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum LiteralChar {
    Normal(char),
    Escaped(char),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Span {
    Literal(String),
    LiteralChars(Vec<LiteralChar>),
    // ~, ~mike, ...
    Tilde(Option<String>),
    // $foo, ${foo}, ${foo:-default}, ...
    Parameter {
        name: String,
        op: ExpansionOp,
        quoted: bool,
    },
    // $${foo[1]} ...
    ArrayParameter {
        name: String,
        index: Word,
        quoted: bool,
    },
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
        expr: Expr,
    },
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
pub struct HereDoc(Vec<Vec<Word>>);

impl HereDoc {
    pub fn lines(&self) -> &[Vec<Word>] {
        &self.0
    }
}
