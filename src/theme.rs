use ansi_term::{Style, Color};

pub struct PromptTheme {
    pub prompt: Style,
    pub prompt_continue: Style,

    pub path: Style,
    pub path_debug: Style,
    pub path_basename: Style,
    pub path_nowrite: Style,
    pub path_nowrite_basename: Style,

    pub repo: Style,
    pub repo_work_tree: Style,
    pub repo_dirty: Style,
    pub repo_staged: Style,

    pub hostname: Style,
    pub username: Style,
}

impl PromptTheme {
    pub fn arrow(style: Style, connected: Option<Style>) -> Style {
        let mut res = if let Some(background) = style.background {
            background.normal()
        } else {
            Style::default()
        };

        if let Some(connected) = connected {
            if let Some(background) = connected.background {
                res = res.on(background);
            }
        }

        res
    }

    pub fn path(&self, readonly: bool) -> Style {
        if readonly {
            self.path_nowrite
        } else {
            self.path
        }
    }

    pub fn basename(&self, readonly: bool) -> Style {
        if readonly {
            self.path_nowrite_basename
        } else {
            self.path_basename
        }
    }
}

pub fn basic_theme() -> PromptTheme {
    let base0d = Color::RGB(0xEB, 0xCB, 0x8B);
    PromptTheme {
        prompt: Color::Purple.bold(),
        prompt_continue: Color::Fixed(13).bold(),

        path: Color::Blue.normal(),
        path_debug: Color::Fixed(83).normal(),
        path_basename: Color::Blue.bold(),
        path_nowrite: Color::Yellow.normal(),
        path_nowrite_basename: Color::Yellow.bold(),

        repo: Color::Green.normal(),
        repo_work_tree: Color::Fixed(15).normal(),
        repo_dirty: Color::Red.normal(),
        repo_staged: Color::Yellow.normal(),

        hostname: base0d.clone().normal(),
        username: base0d.clone().bold(),
    }
}

pub fn nord_theme() -> PromptTheme {
    let base00 = Color::RGB(0x2E, 0x34, 0x40);
    let base02 = Color::RGB(0x43, 0x4C, 0x5E);
    let base05 = Color::RGB(0xE5, 0xE9, 0xF0);
    let base08 = Color::RGB(0x88, 0xC0, 0xD0);
    let base0d = Color::RGB(0xEB, 0xCB, 0x8B);

    PromptTheme {
        prompt: Color::Fixed(10).on(Color::Black).bold(),
        prompt_continue: Color::Fixed(13).on(Color::Black).bold(),

        path: base05.on(base02),
        path_debug: Color::Fixed(83).normal(),
        path_basename: Color::RGB(0xEC, 0xEF, 0xF4).on(base02).bold(),
        path_nowrite: base08.on(base02),
        path_nowrite_basename: base08.on(base02.clone()).bold(),

        repo: base00.on(Color::RGB(0xA3, 0xBE, 0x8C)),
        repo_work_tree: base00.on(base02.clone()).bold(),
        repo_dirty: base00.on(Color::RGB(0xBF, 0x61, 0x6A)),
        repo_staged: base00.on(base0d.clone()),

        hostname: Color::RGB(0x25, 0x5e, 0x87).on(Color::RGB(0xcc, 0xcc, 0xcc)),
        username: Color::RGB(0x25, 0x5e, 0x87).on(Color::RGB(0xcc, 0xcc, 0xcc)).bold(),
    }
}

pub struct SyntaxTheme {
    pub normal: Style,
    pub command: Style,
    pub error: Style,
    pub quoted: Style,
    pub valid_path: Style,
    pub argument: Style,
}

pub fn default_syntax_theme() -> SyntaxTheme {
    SyntaxTheme {
        normal: Style::new(),
        command: Color::RGB(0, 0x5f, 0xd7).normal(),
        error: Color::Red.normal(),
        quoted: Color::RGB(0x99, 0x99, 0x00).normal(),
        valid_path: Style::new().underline(),
        argument: Color::RGB(0x00, 0xaf, 0xff).normal(),
    }
}
