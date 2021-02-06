use std::fs::File;
use std::os::unix::io::FromRawFd;
use std::io::Read;

use crate::shell;
use crate::process;
use crate::variable::Value;

use nix::unistd::{close, pipe};

#[cfg(test)]
fn equal_script(script: &str, expect: &str) {
    let mut shell = shell::Shell::new();

    for (key, value) in std::env::vars() {
        shell.set(&key, Value::String(value.to_owned()), false);
    }

    shell.scan_commands();

    let (pipe_in, pipe_out) = pipe().expect("failed to create a pipe");

    let status = shell.run_str_with_stdio(script, false, 0, pipe_out, 2);
    assert_eq!(status, process::ExitStatus::ExitedWith(0));
    close(pipe_out).ok();

    let mut output = String::new();
    unsafe {
        let mut file = File::from_raw_fd(pipe_in);
        file.read_to_string(&mut output).ok();
    }

    assert_eq!(output, expect);

    close(pipe_in).ok();
}

#[test]
fn test_completion() {
    equal_script(
r##"# 2.2.1 Escape Character (Backslash)
# The following must be quoted:
printf '%s\n' \|\&\;\<\>\(\)\$\`\\\"\'
# The following must be quoted only under certain conditions:
printf '%s\n' *?[#˜=%
# Escaping whitespace:
printf 'arg one: %s; arg two: %s\n' with\ space with\	tab
printf 'arg one: %s; arg two: %s\n' without \
    newline
printf 'arg one: %s; arg two: %s\n' without\
whitespace 'arg two'

# 2.2.2 Single quotes
printf '%s\n' '|&;<>()$`\"'

# 2.2.3 Double quotes
printf '%s\n' "|&;<>()'\""

# Parameter & arithmetic expansion should work:
lval=2
rval=3
printf '%s\n' "$lval + ${rval} = $((lval+rval))"
# Command expansion should work
printf '%s\n' "cmd 1: $(echo "cmd 1")"
printf '%s\n' "cmd 2: `echo "cmd 2"`"
# Command expansion should work, recursively
printf '%s\n' "cmd 3: $(echo $(echo "cmd 3"))"

# Backquotes should work
printf '%s\n' "cmd 4: `echo "cmd 4"`"

# Backslashes should escape the following:
printf '%s\n' "\$\`\"\\\
test"
# But not the following:
printf '%s\n' "\|\&\;\<\>\(\)\'"
"##,

r##"|&;<>()$`\"'
*?[#˜=%
arg one: with space; arg two: with	tab
arg one: without; arg two: newline
arg one: withoutwhitespace; arg two: arg two
|&;<>()$`\"
|&;<>()'"
2 + 3 = 5
cmd 1: cmd 1
cmd 2: cmd 2
cmd 3: cmd 3
cmd 4: cmd 4
$`"\test
\|\&\;\<\>\(\)\'
"##);
}
