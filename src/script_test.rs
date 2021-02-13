use crate::shell;
use crate::variable::Value;

#[cfg(test)]
fn equal_script(script: &str, expect: &str) {
    let mut shell = shell::Shell::new();

    for (key, value) in std::env::vars() {
        shell.set(&key, Value::String(value.to_owned()), false);
    }

    shell.scan_commands();

    let (_, output) = shell.run_to_string(script);
    assert_eq!(output, expect);
}

#[test]
fn test_completion() {
    equal_script(
r##"
unset LUNCH_MENU_CHOICES
LUNCH_MENU_CHOICES=(${LUNCH_MENU_CHOICES[@]} aosp_arm aosp_arm64-eng)
echo ${LUNCH_MENU_CHOICES[@]}
"##,
r##"aosp_arm aosp_arm64-eng
"##,
    );

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
