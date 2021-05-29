# Rushell
`rushell` is a POSIX compatible shell written by Rust language for Linux and MacOS.
It is still under development and it is not fully compatible with Bash.

## Features
* Suggestions
* Two default fancy prompt
* POSIX compatible shell (but not perfect yet)
* Support bash style completer (completer and compgen commands, but not implemented full feature yet.)
* Support git status prompt and alias of git commands

## Config Folder
- init script
  - $HOME/.config/rushell/init.sh
- History
  - $HOME/.config/rushell/history

## Prompt

Use PROMPT_STYLE environment in $HOME/.config/rushell/init.sh

`PROMPT_STYLE=power`

![power prompt](https://raw.githubusercontent.com/hiking90/rushell/master/prompt_power_screenshot.png)

`PROMPT_STYLE=basic`

![basic prompt](https://raw.githubusercontent.com/hiking90/rushell/master/prompt_basic_screenshot.png)

<!--
And, if you want to use external prompt tool like starship, you can use PROMPT_COMMAND environment in init.sh.

`PROMPT_COMMAND="startship prompt"`
 -->

## Example init script
This example requires "bat", "exa" and "zoxide" packages.
<pre>
export PATH=~/bin:~/.cargo/bin:/usr/local/bin:$PATH
PROMPT_STYLE=power
alias less=bat
alias ls=exa
eval "$(zoxide init posix --hook prompt)"
</pre>

## Related Open Source
`rushell` was started from nsh(https://github.com/nuta/nsh).
And, it is heavily depend on forked linefeed(https://github.com/murarth/linefeed).

And, it uses a lot of open sources written by Rust.

## Contributing

Please feel free to summit issues if you want to suggest features or if you find any critical bugs.

## License

`rushell` is distributed under the terms of both the MIT license and the Apache License (Version 2.0).