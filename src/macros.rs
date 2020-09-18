lazy_static! {
    pub static ref COLORS_ENABLED: bool = {
        use std::os::unix::io::AsRawFd;
        nix::unistd::isatty(std::io::stdout().as_raw_fd()).unwrap_or(false)
    };
}

#[macro_export]
macro_rules! print_err {
    () => { eprintln!(""); };
    ($fmt:expr) => {
        if *crate::macros::COLORS_ENABLED {
            eprintln!("{}", ::ansi_term::Colour::Yellow.bold().paint(concat!("rushell: ", $fmt)));
        } else {
            eprintln!(concat!("rushell: ", $fmt));
        }
    };
    () => { eprintln!(""); };
    ($fmt:expr, $($arg:tt)*) => {
        if *crate::macros::COLORS_ENABLED {
            eprintln!(concat!("{}rushell: ", $fmt, "{}"),
                ::ansi_term::Colour::Yellow.bold().prefix().to_owned(),
                $($arg)*,
                ::ansi_term::Style::default().suffix().to_owned());
        } else {
            eprintln!(concat!("rushell: ", $fmt), $($arg)*);
        }
    };
}
