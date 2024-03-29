use crate::builtins::InternalCommandError;
use crate::expand::*;
use crate::parser::{
    self, Assignment, Ast, BinaryExpr, CondExpr, Expr, HereDoc, Initializer, LocalDeclaration,
    RunIf, Word,
};
use crate::pattern::{match_pattern_all, NoMatchesError};
use crate::process::*;
use crate::shell::Shell;
use crate::variable::Value;
use crate::utils::{Error, Result};
use nix;
use nix::unistd::{close, fork, pipe, setpgid, ForkResult, Pid};
use regex::Regex;
use std::fs::File;
use std::path::PathBuf;
use std::io::prelude::*;
use std::os::unix::io::FromRawFd;
use std::os::unix::io::RawFd;

macro_rules! bool_to_int {
    ($e:expr) => {
        if $e {
            1
        } else {
            0
        }
    };
}

pub fn evaluate_expr(shell: &mut Shell, expr: &Expr) -> i32 {
    match expr {
        // Expr::Expr(sub_expr) => evaluate_expr(shell, sub_expr),
        Expr::Literal(value) => *value,
        Expr::Word(word) => {
            if let Ok(name) = expand_word_into_string(shell, word) {
                shell.get_var_as_i32(&name).unwrap_or(0)
            } else {
                0
            }
        },
        Expr::Minus(sub_expr) => - evaluate_expr(shell, sub_expr),
        Expr::Parameter { name } => shell.get_var_as_i32(&name).unwrap_or(0),
        Expr::Add(BinaryExpr { lhs, rhs }) => evaluate_expr(shell, lhs) + evaluate_expr(shell, rhs),
        Expr::Sub(BinaryExpr { lhs, rhs }) => evaluate_expr(shell, lhs) - evaluate_expr(shell, rhs),
        Expr::Mul(BinaryExpr { lhs, rhs }) => evaluate_expr(shell, lhs) * evaluate_expr(shell, rhs),
        Expr::Div(BinaryExpr { lhs, rhs }) => evaluate_expr(shell, lhs) / evaluate_expr(shell, rhs),
        Expr::Modulo(BinaryExpr { lhs, rhs }) => evaluate_expr(shell, lhs) % evaluate_expr(shell, rhs),
        Expr::Assign { name, rhs } => {
            let value = evaluate_expr(shell, rhs);
            shell.assign(&name, Value::String(value.to_string()));
            value
        }
        Expr::Eq(lhs, rhs) => bool_to_int!(evaluate_expr(shell, lhs) == evaluate_expr(shell, rhs)),
        Expr::Ne(lhs, rhs) => bool_to_int!(evaluate_expr(shell, lhs) != evaluate_expr(shell, rhs)),
        Expr::Lt(lhs, rhs) => bool_to_int!(evaluate_expr(shell, lhs) < evaluate_expr(shell, rhs)),
        Expr::Le(lhs, rhs) => bool_to_int!(evaluate_expr(shell, lhs) <= evaluate_expr(shell, rhs)),
        Expr::Gt(lhs, rhs) => bool_to_int!(evaluate_expr(shell, lhs) > evaluate_expr(shell, rhs)),
        Expr::Ge(lhs, rhs) => bool_to_int!(evaluate_expr(shell, lhs) >= evaluate_expr(shell, rhs)),
        Expr::Inc(name) => {
            let value = shell.get_var_as_i32(&name).unwrap_or(0) + 1;
            shell.assign(&name, Value::String(value.to_string()));
            value
        }
        Expr::Dec(name) => {
            let value = shell.get_var_as_i32(&name).unwrap_or(0) - 1;
            shell.assign(&name, Value::String(value.to_string()));
            value
        }
    }
}

pub fn evaluate_regex(_shell: &Shell, text: String, pattern: &str) -> bool {
    match Regex::new(pattern) {
        Ok(re) => {
            if re.is_match(&text) {
                true
            } else {
                false
            }
        }
        Err(err) => {
            print_err!("{}", err);
            false
        }
    }
}

pub fn evaluate_file(_shell: &Shell, file: String, op: &str) -> bool {
    let path = PathBuf::from(file);
    match op {
        "-f" => {
            path.is_file()
        }
        "-a" | "-e" => {
            path.exists()
        }
        "-d" => {
            path.is_dir()
        }
        "-h" | "-L" => {
            if let Ok(meta) = path.symlink_metadata() {
                meta.file_type().is_symlink()
            } else {
                false
            }
        }
        "-s" => {
            if let Ok(meta) = path.symlink_metadata() {
                meta.len() > 0
            } else {
                false
            }
        }
        "-N" => {
            if let Ok(meta) = path.symlink_metadata() {
                let modified = meta.modified().ok();
                let accessed = meta.accessed().ok();
                if modified != None && accessed != None {
                    modified > accessed
                } else {
                    false
                }
            } else {
                false
            }
        }
        "-b" | "-c" | "-g" |
        "-k" | "-p" | "-r" | "-t" | "-u" |
        "-w" | "-x" | "-O" | "-G" |
        "-S" => {
            false
        }
        _ => {
            print_err!("Unknown primary operator {}", op);
            false
        }
    }
}

/// Evaluates a variable initializer.
///
/// ```
/// this_is_string=hello_world
///                ^^^^^^^^^^^ a string initializer
/// this_is_array=(a b c)
///               ^^^^^^^ an array initializer
/// ```
///
pub fn evaluate_initializer(shell: &mut Shell, initializer: &Initializer) -> Result<Value> {
    match initializer {
        Initializer::String(ref word) => Ok(Value::String(expand_word_into_string(shell, word)?)),
        Initializer::Array(ref words) => {
            let mut elems = expand_words(shell, words)?;
            elems = elems.into_iter().filter(|e| e.len() > 0).collect();
            match (elems.len(), elems.get(0)) {
                (1, Some(ref body)) if body.is_empty() => {
                    // Make `foo=()' an empty array.
                    Ok(Value::Array(vec![]))
                }
                _ => Ok(Value::Array(elems)),
            }
        }
    }
}

pub fn evaluate_heredoc(shell: &mut Shell, heredoc: &HereDoc) -> Result<RawFd> {
    let mut lines = Vec::new();
    for line in heredoc.lines() {
        lines.push(expand_word_into_string(shell, line)?);
    }

    let mut body = lines.join("\n");
    body += "\n";

    let (pipe_out, pipe_in) = pipe().expect("failed to create a pipe");
    unsafe {
        let mut file = File::from_raw_fd(pipe_in);
        file.write_all(body.as_bytes()).ok();
        // Ensure that pipe_in is closed.
        drop(file);
    };

    Ok(pipe_out)
}

fn call_function(
    shell: &mut Shell,
    name: &str,
    ctx: &Context,
    args: &[String],
    locals: Vec<(&str, Value)>,
) -> Result<ExitStatus> {
    if let Some(var) = shell.get(name) {
        if let Some(Value::Function(ref body)) = var.value() {
            shell.enter_frame();
            let frame = shell.current_frame_mut();
            // Set local variables.
            for (name, value) in locals {
                frame.set(name, value);
            }

            // $1, $2, ...
            frame.set_args(&args);

            let result = match run_command(shell, &body, ctx)? {
                ExitStatus::Return => ExitStatus::ExitedWith(0),
                result => result,
            };
            shell.leave_frame();
            return Ok(result);
        }
    }

    // No such a function.
    Ok(ExitStatus::ExitedWith(1))
}

fn run_simple_command(
    shell: &mut Shell,
    ctx: &Context,
    argv: &[Word],
    external: bool,
    redirects: &[parser::Redirection],
    assignments: &[parser::Assignment],
) -> Result<ExitStatus> {
    let argv = if external {
        expand_words(shell, argv)?
    } else {
        expand_words(shell, &expand_alias(shell, argv))?
    };
    if argv.is_empty() {
        // `argv` is empty. For example bash accepts `> foo.txt`; it creates an empty file
        // named "foo.txt".
        return Ok(ExitStatus::ExitedWith(0));
    }

    // Functions
    let argv0 = argv[0].to_owned();
    if let Some(var) = shell.get(&argv0) {
        if var.is_function() {
            let args: Vec<String> = argv.iter().skip(1).cloned().collect();
            return call_function(shell, &argv0, ctx, &args, vec![]);
        }
    }
    // Internal commands
    let result = run_internal_command(shell, &argv, ctx.stdin, ctx.stdout, ctx.stderr, redirects);
    match result {
        Ok(status) => return Ok(status),
        Err(err) => {
            // match err.find_root_cause().downcast_ref::<InternalCommandError>() {
            match err.downcast_ref::<InternalCommandError>() {
                Some(InternalCommandError::BadRedirection) => return Ok(ExitStatus::ExitedWith(1)),
                Some(InternalCommandError::NotFound) => (), /* Try external command. */
                _ => return Err(err),
            }
        }
    }

    // External commands
    run_external_command(shell, &ctx, argv, redirects, assignments)
}

fn run_local_command(
    shell: &mut Shell,
    declarations: &[parser::LocalDeclaration],
) -> Result<ExitStatus> {
    if shell.in_global_frame() {
        print_err!("local variable can only be used in a function");
        Ok(ExitStatus::ExitedWith(1))
    } else {
        for decl in declarations {
            if CTRLC.load(Ordering::Relaxed) == true {
                return Ok(EXIT_STATUS_CTRL_C);
            }

            match decl {
                LocalDeclaration::Assignment(Assignment {
                    name, initializer, index: _, append,
                }) => {
                    let value = evaluate_initializer(shell, &initializer)?;
                    if *append == false {
                        shell.set(&name, value, true)
                    } else {
                        shell.append(&name, value, true)
                    }
                }
                LocalDeclaration::Name(name) => shell.define(name, true),
            }
        }

        Ok(ExitStatus::ExitedWith(0))
    }
}

fn run_if_command(
    shell: &mut Shell,
    ctx: &Context,
    condition: &[parser::Term],
    then_part: &[parser::Term],
    elif_parts: &[parser::ElIf],
    else_part: &Option<Vec<parser::Term>>,
    _redirections: &[parser::Redirection],
) -> Result<ExitStatus> {
    // then
    let result = run_terms(shell, condition, ctx.stdin, ctx.stdout, ctx.stderr);
    if result == ExitStatus::ExitedWith(0) {
        return Ok(run_terms(
            shell, then_part, ctx.stdin, ctx.stdout, ctx.stderr,
        ));
    }

    // elif
    for elif in elif_parts {
        let result = run_terms(shell, &elif.condition, ctx.stdin, ctx.stdout, ctx.stderr);
        if result == ExitStatus::ExitedWith(0) {
            return Ok(run_terms(
                shell, &elif.then_part, ctx.stdin, ctx.stdout, ctx.stderr,
            ));
        }
    }

    // else
    if let Some(else_part) = else_part {
        return Ok(run_terms(
            shell, else_part, ctx.stdin, ctx.stdout, ctx.stderr,
        ));
    }

    Ok(ExitStatus::ExitedWith(0))
}

fn run_case_command(
    shell: &mut Shell,
    ctx: &Context,
    word: &parser::Word,
    cases: &[parser::CaseItem],
) -> Result<ExitStatus> {
    let word = expand_word_into_string(shell, word)?;
    // Find the exact match first.
    // If there is no exact matched pattern, find regular expression match.
    for case in cases {
        for pattern in &case.patterns {
            let pattern = expand_into_single_pattern_word(shell, &pattern)?;
            if match_pattern_all(&pattern, &word) {
                return Ok(run_terms(
                    shell, &case.body, ctx.stdin, ctx.stdout, ctx.stderr,
                ));
            }
        }
    }

    Ok(ExitStatus::ExitedWith(0))
}

use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

lazy_static! {
    pub static ref CTRLC: Arc<AtomicBool> = Arc::new(AtomicBool::new(false));
}

fn run_while_command(
    shell: &mut Shell,
    ctx: &Context,
    condition: &[parser::Term],
    body: &[parser::Term],
) -> Result<ExitStatus> {
    let mut last_result = ExitStatus::ExitedWith(0);
    loop {
        if CTRLC.load(Ordering::Relaxed) == true {
            last_result = EXIT_STATUS_CTRL_C;
            break;
        }
        let result = run_terms(shell, condition, ctx.stdin, ctx.stdout, ctx.stderr);
        if result != ExitStatus::ExitedWith(0) {
            break;
        }

        last_result = run_terms(shell, body, ctx.stdin, ctx.stdout, ctx.stderr);
    }

    Ok(last_result)
}

fn run_for_command(
    shell: &mut Shell,
    ctx: &Context,
    var_name: &str,
    words: &[Word],
    body: &[parser::Term],
) -> Result<ExitStatus> {
    'for_loop: for unexpanded_word in words {
        let expanded_words = expand_word_into_vec(shell, unexpanded_word, &shell.ifs())?;
        for pattern_word in expanded_words {
            for value in pattern_word.expand_glob()? {
                if CTRLC.load(Ordering::Relaxed) == true {
                    return Ok(EXIT_STATUS_CTRL_C);
                }

                shell.set(&var_name, Value::String(value), true);

                let result = run_terms(shell, body, ctx.stdin, ctx.stdout, ctx.stderr);
                match result {
                    ExitStatus::Break => break 'for_loop,
                    ExitStatus::Continue => (),
                    ExitStatus::Return => return Ok(result),
                    _ => (),
                }
            }
        }
    }

    Ok(ExitStatus::ExitedWith(0))
}

fn run_arith_for_command(
    shell: &mut Shell,
    ctx: &Context,
    init: &Expr,
    cond: &Expr,
    update: &Expr,
    body: &[parser::Term],
) -> Result<ExitStatus> {
    evaluate_expr(shell, init);

    'for_loop: while evaluate_expr(shell, cond) == 1 {
        if CTRLC.load(Ordering::Relaxed) == true {
            return Ok(EXIT_STATUS_CTRL_C);
        }
        let result = run_terms(shell, body, ctx.stdin, ctx.stdout, ctx.stderr);
        match result {
            ExitStatus::Break => break 'for_loop,
            ExitStatus::Continue => (),
            ExitStatus::Return => return Ok(result),
            _ => (),
        }

        evaluate_expr(shell, update);
    }

    Ok(ExitStatus::ExitedWith(0))
}

fn run_command(shell: &mut Shell, command: &parser::Command, ctx: &Context) -> Result<ExitStatus> {
    if shell.noexec {
        return Ok(ExitStatus::NoExec);
    }

    trace!("run_command: {:?}", command);
    let result = match command {
        parser::Command::SimpleCommand {
            external,
            argv,
            redirects,
            assignments,
        } => run_simple_command(shell, ctx, &argv, *external, &redirects, &assignments)?,
        parser::Command::If {
            condition,
            then_part,
            elif_parts,
            else_part,
            redirects,
        } => run_if_command(
            shell,
            ctx,
            &condition,
            &then_part,
            &elif_parts,
            &else_part,
            &redirects,
        )?,
        parser::Command::While { condition, body } => {
            run_while_command(shell, ctx, &condition, &body)?
        }
        parser::Command::Case { word, cases } => run_case_command(shell, ctx, &word, &cases)?,
        parser::Command::For {
            var_name,
            words,
            body,
        } => run_for_command(shell, ctx, var_name, &words, &body)?,
        parser::Command::ArithFor {
            init,
            cond,
            update,
            body,
        } => run_arith_for_command(shell, ctx, init, cond, update, &body)?,
        parser::Command::LocalDef { declarations } => run_local_command(shell, &declarations)?,
        parser::Command::FunctionDef { name, body } => {
            shell.set(name, Value::Function(body.clone()), true);
            ExitStatus::ExitedWith(0)
        }
        parser::Command::Assignment { assignments } => {
            for assign in assignments {
                let value = evaluate_initializer(shell, &assign.initializer)?;
                if assign.append {
                    shell.assign_append(&assign.name, value)
                } else {
                    shell.assign(&assign.name, value)
                }
            }
            ExitStatus::ExitedWith(0)
        }
        parser::Command::Cond{is_not, expr} => {
            let mut status = 1;
            if let Some(expr) = expr {
                let mut result = evaluate_cond(shell, expr)?;
                if *is_not {
                    result = !result;
                }
                if result {
                    status = 0;
                }
            }
            ExitStatus::ExitedWith(status)
        }
        parser::Command::Group { terms } => {
            run_terms(shell, terms, ctx.stdin, ctx.stdout, ctx.stderr)
        }
        parser::Command::SubShellGroup { terms } => {
            let pid = spawn_subshell(shell, terms, ctx)?;
            let status = wait_child(pid).unwrap_or(1);
            ExitStatus::ExitedWith(status)
        }
        parser::Command::Return { status } => {
            if let Some(status) = status {
                if let Ok(status) = expand_word_into_string(shell, status) {
                    if let Ok(status) = status.parse::<i32>() {
                        shell.set_last_status(status);
                    }
                }
            }

            ExitStatus::Return
        }
        parser::Command::Break => ExitStatus::Break,
        parser::Command::Continue => ExitStatus::Continue,
    };

    Ok(result)
}

/// Runs commands in a subshell (`$()` or `<()`).
pub fn eval_in_subshell(shell: &mut Shell, terms: &[parser::Term]) -> Result<(i32, File)> {
    let (pipe_out, pipe_in) = pipe().expect("failed to create a pipe");

    let ctx = Context {
        stdin: 0,
        stdout: pipe_in,
        stderr: 2,
        pgid: None,
        background: false,
        interactive: false,
    };

    let pid = spawn_subshell(shell, terms, &ctx)?;
    close(pipe_in).ok();
    let status = wait_child(pid).unwrap_or(1);
    unsafe {
        Ok((status, File::from_raw_fd(pipe_out)))
    }
}

pub fn eval_completion_function(shell: &mut Shell, name: &str, line: &str, words: Vec<String>, current: usize) -> Vec<String> {
    let mut res = Vec::new();

    if let Some(var) = shell.get(name) {
        if let Some(Value::Function(ref body)) = var.value() {
            let (pipe_out, pipe_in) = pipe().expect("failed to create a pipe");

            let ctx = Context {
                stdin: 0,
                stdout: pipe_in,
                stderr: 2,
                pgid: None,
                background: false,
                interactive: false,
            };

            shell.enter_frame();

            {
                let frame = shell.current_frame_mut();

                let mut args = [String::new(), String::new(), String::new()];
                for i in 0..words.len() {
                    if i == 0 {
                        args[0] = words[i].to_owned();
                    } else if i == current {
                        args[1] = words[i].to_owned();
                    }
                    if i + 1 == current {
                        args[2] = words[i].to_owned();
                    }
                }
                // $1, $2, ...
                frame.set_args(&args);

                shell.set("COMP_WORDS", Value::Array(words), true);
                shell.set("COMP_CWORD", Value::String(current.to_string()), true);
                shell.set("COMP_LINE", Value::String(line.into()), true);

                shell.set("COMPREPLY", Value::Array(vec![]), true);
            }

            if let Ok(result) = run_command(shell, &body, &ctx) {
                match result {
                    ExitStatus::ExitedWith(_) |
                    ExitStatus::Return => {
                        if let Some(value) = shell.get("COMPREPLY") {
                            value.array().map(|a| a.for_each(|v| res.push(v.into())));
                        }
                    }
                    _ => {}
                }
            }
            shell.leave_frame();

            close(pipe_in).ok();
            close(pipe_out).ok();
        }
    }

    res
}

fn spawn_subshell(shell: &mut Shell, terms: &[parser::Term], ctx: &Context) -> Result<Pid> {
    match unsafe { fork() }.expect("failed to fork") {
        ForkResult::Parent { child } => Ok(child),
        ForkResult::Child => {
            let status = match run_terms(shell, terms, ctx.stdin, ctx.stdout, ctx.stderr) {
                ExitStatus::ExitedWith(status) => status,
                _ => 1,
            };

            std::process::exit(status);
        }
    }
}

// Creates a pipeline and runs commands.
fn run_pipeline(
    shell: &mut Shell,
    code: &str,
    pipeline: &parser::Pipeline,
    pipeline_stdin: RawFd,
    pipeline_stdout: RawFd,
    stderr: RawFd,
    background: bool,
) -> ExitStatus {
    // Invoke commands in a pipeline.
    let mut last_result = None;
    let mut iter = pipeline.commands.iter().peekable();
    let mut childs = Vec::new();
    let mut stdin = pipeline_stdin;
    let mut pgid = None;
    while let Some(command) = iter.next() {
        if CTRLC.load(Ordering::Relaxed) == true {
            last_result = Some(EXIT_STATUS_CTRL_C);
            break;
        }

        let stdout;
        let pipes = if iter.peek().is_some() {
            // There is a next command in the pipeline (e.g. date in
            // `date | hexdump`). Create and connect a pipe.
            let (pipe_out, pipe_in) = pipe().expect("failed to create a pipe");
            stdout = pipe_in;
            Some((pipe_out, pipe_in))
        } else {
            // The last command in the pipeline.
            stdout = pipeline_stdout;
            None
        };

        let result = run_command(
            shell,
            command,
            &Context {
                stdin,
                stdout,
                stderr,
                pgid,
                background,
                interactive: shell.interactive(),
            },
        );

        if let Some((pipe_out, pipe_in)) = pipes {
            stdin = pipe_out;
            // `pipe_in` is used by a child process and is no longer needed.
            close(pipe_in).expect("failed to close pipe_in");
        }

        last_result = match result {
            Ok(ExitStatus::Running(pid)) => {
                if pgid.is_none() {
                    // The first child (the process group leader) pid is used for pgid.
                    pgid = Some(pid);
                }

                if shell.interactive {
                    setpgid(pid, pgid.unwrap()).expect("failed to setpgid");
                }

                childs.push(pid);
                Some(ExitStatus::Running(pid))
            }
            Ok(ExitStatus::ExitedWith(status)) => Some(ExitStatus::ExitedWith(status)),
            Ok(status) => {
                last_result = Some(status);
                break;
            }
            // Ok(ExitStatus::Break) => {
            //     last_result = Some(ExitStatus::Break);
            //     break;
            // }
            // Ok(ExitStatus::Continue) => {
            //     last_result = Some(ExitStatus::Continue);
            //     break;
            // }
            // Ok(ExitStatus::Return) => {
            //     last_result = Some(ExitStatus::Return);
            //     break;
            // }
            // Ok(ExitStatus::NoExec) => {
            //     last_result = Some(ExitStatus::NoExec);
            //     break;
            // }
            // Ok(ExitStatus::Expected) => {
            //     last_result = Some(ExitStatus::Expected);
            //     break;
            // }
            Err(err) => {
                if err
                    .downcast_ref::<NoMatchesError>()
                    .is_some()
                {
                    print_err!("error: no matches");
                    last_result = Some(ExitStatus::ExitedWith(1));
                } else {
                    print_err!("{:?}", err);
                }
                break;
            }
        };
    }

    // Wait for the last command in the pipeline.
    let last_status = match last_result {
        Some(ExitStatus::ExitedWith(status)) => {
            shell.set_last_status(status);
            ExitStatus::ExitedWith(status)
        }
        Some(ExitStatus::Running(_)) => {
            let cmd_name = code.to_owned();
            let job = shell.create_job(cmd_name, pgid.unwrap(), childs);

            if !shell.interactive {
                if background {
                    // Update `$!`.
                    run_in_background(shell, &job, false);
                }

                match wait_for_job(shell, &job) {
                    ProcessState::Completed(status) => {
                        shell.set_last_status(status);
                        ExitStatus::ExitedWith(status)
                    }
                    ProcessState::Stopped(_) => ExitStatus::Running(pgid.unwrap()),
                    _ => unreachable!(),
                }
            } else if background {
                run_in_background(shell, &job, false);
                ExitStatus::Running(pgid.unwrap())
            } else {
                match run_in_foreground(shell, &job, false) {
                    ProcessState::Completed(status) => ExitStatus::ExitedWith(status),
                    ProcessState::Stopped(_) => ExitStatus::Running(pgid.unwrap()),
                    _ => unreachable!(),
                }
            }
        }
        Some(status) => {
            return status;
        }
        // Some(ExitStatus::Break) => {
        //     return ExitStatus::Break;
        // }
        // Some(ExitStatus::Continue) => {
        //     return ExitStatus::Continue;
        // }
        // Some(ExitStatus::Return) => {
        //     return ExitStatus::Return;
        // }
        // Some(ExitStatus::NoExec) => {
        //     return ExitStatus::NoExec;
        // }
        // Some(ExitStatus::NoExec) => {
        //     return ExitStatus::NoExec;
        // }
        None => {
            trace!("nothing to execute");
            ExitStatus::ExitedWith(0)
        }
    };

    if shell.errexit {
        if let ExitStatus::ExitedWith(status) = last_status {
            if status != 0 {
                std::process::exit(status);
            }
        }
    }

    last_status
}

/// Runs pipelines.
pub fn run_terms(
    shell: &mut Shell,
    terms: &[parser::Term],
    stdin: RawFd,
    stdout: RawFd,
    stderr: RawFd,
) -> ExitStatus {
    let mut last_status = ExitStatus::ExitedWith(0);
    for term in terms {
        // println!("{:?}", term);
        for pipeline in &term.pipelines {
            if CTRLC.load(Ordering::Relaxed) == true {
                last_status = EXIT_STATUS_CTRL_C;
                break;
            }
            // Should we execute the pipline?
            match (last_status, &pipeline.run_if) {
                (ExitStatus::ExitedWith(0), RunIf::Success) => (),
                (ExitStatus::ExitedWith(0), RunIf::Failure) => continue,
                (ExitStatus::ExitedWith(_), RunIf::Failure) => (),
                (ExitStatus::Break, _) => return ExitStatus::Break,
                (ExitStatus::Continue, _) => return ExitStatus::Continue,
                (ExitStatus::Return, _) => return ExitStatus::Return,
                (_, RunIf::Always) => (),
                _ => continue,
            }

            last_status = run_pipeline(
                shell,
                &term.code,
                pipeline,
                stdin,
                stdout,
                stderr,
                term.background,
            );
        }
    }

    last_status
}

/// Run CondEx command (`[[ ... ]]`).
pub fn evaluate_cond_primary(shell: &mut Shell, cond: &CondExpr) -> Result<String> {
    match cond {
        CondExpr::Word(word) => expand_word_into_string(shell, word),
        _ => Err(Error::Message(format!("cond: expected word")).into()),
    }
}

/// Run CondEx command (`[[ ... ]]`).
pub fn evaluate_cond(shell: &mut Shell, cond: &CondExpr) -> Result<bool> {
    macro_rules! eval_as_string {
        ($expr:expr) => {
            evaluate_cond_primary(shell, $expr)?
        };
    }

    macro_rules! unwrap_word {
        ($expr:expr) => {
            match $expr {
                CondExpr::Word(word) => word,
                _ => {
                    return Err(Error::Message(format!("cond: expected word")).into());
                }
            }
        };
    }

    macro_rules! eval_as_bool {
        ($expr:expr) => {
            evaluate_cond(shell, $expr)?
        };
    }

    macro_rules! parse_as_int {
        ($expr:expr) => {
            evaluate_cond_primary(shell, $expr)?.parse().unwrap_or(0) as i32
        };
    }

    let result = match cond {
        CondExpr::And(lhs, rhs) => eval_as_bool!(lhs) && eval_as_bool!(rhs),
        CondExpr::Or(lhs, rhs) => eval_as_bool!(lhs) || eval_as_bool!(rhs),
        CondExpr::StrEq(lhs, rhs) => {
            let pat = expand_into_single_pattern_word(shell, unwrap_word!(rhs.as_ref()))?;
            match_pattern_all(&pat, &eval_as_string!(lhs))
        }
        CondExpr::StrNe(lhs, rhs) => {
            let pat = expand_into_single_pattern_word(shell, unwrap_word!(rhs.as_ref()))?;
            !match_pattern_all(&pat, &eval_as_string!(lhs))
        }
        CondExpr::Eq(lhs, rhs) => parse_as_int!(lhs) == parse_as_int!(rhs),
        CondExpr::Ne(lhs, rhs) => parse_as_int!(lhs) != parse_as_int!(rhs),
        CondExpr::Lt(lhs, rhs) => parse_as_int!(lhs) < parse_as_int!(rhs),
        CondExpr::Le(lhs, rhs) => parse_as_int!(lhs) <= parse_as_int!(rhs),
        CondExpr::Gt(lhs, rhs) => parse_as_int!(lhs) > parse_as_int!(rhs),
        CondExpr::Ge(lhs, rhs) => parse_as_int!(lhs) >= parse_as_int!(rhs),
        CondExpr::Regex(lhs, rhs) => {
            let text = eval_as_string!(lhs);
            evaluate_regex(shell, text, rhs)
        }
        CondExpr::File(lhs, op) => {
            let text = eval_as_string!(lhs);
            evaluate_file(shell, text, op)
        }
        CondExpr::Word(_) => true,
    };

    Ok(result)
}

/// Runs commands.
pub fn eval(
    shell: &mut Shell,
    ast: &Ast,
    stdin: RawFd,
    stdout: RawFd,
    stderr: RawFd,
) -> ExitStatus {
    trace!("ast: {:#?}", ast);
    run_terms(shell, &ast.terms, stdin, stdout, stderr)
}

#[test]
fn test_expr() {
    let mut shell = Shell::new_for_test();
    assert_eq!(
        evaluate_expr(
            &mut shell,
            &&Expr::Mul(BinaryExpr {
                lhs: Box::new(Expr::Literal(2)),
                rhs: Box::new(Expr::Add(BinaryExpr {
                    lhs: Box::new(Expr::Literal(3)),
                    rhs: Box::new(Expr::Literal(7)),
                })),
            })
        ),
        2 * (3 + 7)
    );

    shell.set("x", Value::String(3.to_string()), false);
    assert_eq!(
        evaluate_expr(
            &mut shell,
            &&Expr::Add(BinaryExpr {
                lhs: Box::new(Expr::Literal(1)),
                rhs: Box::new(Expr::Add(BinaryExpr {
                    lhs: Box::new(Expr::Mul(BinaryExpr {
                        lhs: Box::new(Expr::Literal(2)),
                        rhs: Box::new(Expr::Parameter { name: "x".into() }),
                    })),
                    rhs: Box::new(Expr::Literal(4)),
                })),
            })
        ),
        1 + 2 * 3 + 4
    );
}
