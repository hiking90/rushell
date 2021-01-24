use nix::unistd;
use std::io::Write;
use std::os::unix::io::RawFd;
use std::path::PathBuf;
use std::env;

use err_derive::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error(display = "Error message: {:?}", _0)]
    Message(String),
}

use std::error;
pub type Result<I> = std::result::Result<I, Box<dyn error::Error>>;

pub const SHELL_NAME: &'static str = "rushell";

/// `File`-like object but does not close the `fd`.
pub struct FdFile {
    fd: RawFd,
}

impl FdFile {
    pub fn new(fd: RawFd) -> FdFile {
        FdFile { fd }
    }

    pub fn read_line(&self) -> Option<String> {
        let mut line = Vec::new();
        loop {
            let mut ch = vec![0; 1];
            match unistd::read(self.fd, &mut ch) {
                // EOF
                Ok(read_len) if read_len == 0 => break,
                // Read a character.
                Ok(_) => {
                    if ch[0] == 0x0a
                    /* newline */
                    {
                        break;
                    }

                    line.push(ch[0]);
                }
                // Something went wrong.
                Err(err) => {
                    trace!("read_line: error: {:?}", err);
                    break;
                }
            }
        }

        if line.is_empty() {
            None
        } else {
            Some(String::from_utf8(line).expect("binary data is not yet supported"))
        }
    }
}

impl Write for FdFile {
    #[inline]
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        let len = unistd::write(self.fd, buf).expect("failed to write");
        Ok(len)
    }

    #[inline]
    fn flush(&mut self) -> std::io::Result<()> {
        unistd::fsync(self.fd).ok();
        Ok(())
    }
}

pub fn home_dir() -> PathBuf {
    dirs::home_dir().unwrap_or(PathBuf::from("/"))
}

pub fn current_working_dir() -> PathBuf {
    env::current_dir().unwrap_or(PathBuf::from("/"))
}

pub fn var_os(env: &str, default: &str) -> String {
    if let Some(value) = env::var_os(env) {
        if let Ok(value) = value.into_string() {
            return value;
        }
    }

    default.to_owned()
}

pub fn home_dir_for_user(user: &str) -> Option<String> {
    use nix::libc::getpwnam;
    use std::ffi::{CString, CStr};

    let user = CString::new(user).ok()?;

    unsafe {
        let passwd = getpwnam(user.as_ptr());
        return Some(CStr::from_ptr((*passwd).pw_dir).to_str().ok()?.to_string());
    }
}
