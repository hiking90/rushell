use ansi_term::{Style, Color};

pub enum PathColor {
    ReadWrite = 0,
    ReadOnly,
    Max,
}

pub enum GitColor {
    Unstaged = 0,
    Staged,
    Committed,
    Max
}

pub struct Theme {
    pub prompt: Style,
    pub path: [Style; PathColor::Max as usize],
    pub git: [Style; GitColor::Max as usize],
}

pub fn default_theme() -> Theme {
    Theme {
        prompt: Color::Fixed(10).bold(),
        path: [Color::Fixed(87).bold(), Color::Red.bold()],
        git: [Color::Fixed(196).bold(), Color::Fixed(11).bold(), Color::Fixed(82).bold()],
    }
}

