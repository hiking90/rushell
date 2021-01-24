use regex::Regex;
use crate::glob;
use crate::utils::Result;
use err_derive::Error;

#[derive(Debug, Error)]
#[error(display = "No matches error")]
pub struct NoMatchesError;

/// A word which includes patterns. We don't expand words
/// into the `Vec<String>` directly since the patterns has
/// two different meanings: path glob and match in `case`.
#[derive(Debug)]
pub struct PatternWord {
    fragments: Vec<String>,
}

impl PatternWord {
    pub fn new(fragments: Vec<String>) -> PatternWord {
        PatternWord {
            fragments: fragments,
        }
    }

    pub fn fragments(&self) -> &[String] {
        &self.fragments
    }

    /// Returns a string. Pattern characters such as `*` are treated as a literal.
    pub fn into_string(&self) -> String {
        let mut string = String::new();
        for frag in &self.fragments {
            string += frag;
        }

        string
    }

    pub fn regex(&self, match_all:bool) -> Option<Regex> {
        match glob::glob_to_regex(&self.into_string(), match_all) {
            Some((_, regex, _)) => Some(regex),
            None => None,
        }
    }

    //// Expand patterns as a file path globbing.
    pub fn expand_glob(self) -> Result<Vec<String>> {
        let mut expanded_words = Vec::new();

        let pattern = self.into_string();

        // if glob::includes_glob(&pattern) {
        //     if let Some(mut glob_paths) = glob::glob(&pattern) {
        //         expanded_words.append(&mut glob_paths);
        //     }
        // }
        if let Some(mut glob_paths) = glob::glob(&pattern) {
            expanded_words.append(&mut glob_paths);
        }
        if expanded_words.is_empty() {
            expanded_words.push(pattern);
        }

        Ok(expanded_words)
    }
}

#[derive(Debug, PartialEq)]
pub struct MatchResult {
    start: usize,
    end: usize,
}

fn pattern_word_match(pattern: &PatternWord, text: &str, match_all: bool) -> Option<MatchResult> {
    trace!("pattern_word_match: text = '{}'", text);
    let regex = pattern.regex(match_all);

    match regex {
        Some(re) => {
            if let Some(res) = re.find(text) {
                if res.start() < res.end() {
                    return Some(MatchResult{
                        start: res.start(),
                        end: res.end(),
                    });
                }
            } else {
                // println!("{:?}", re);
            }
        }
        None => {
            // println!("{:?}", err);
        }
    }

    None
}

// pub fn match_pattern(pattern: &PatternWord, text: &str) -> bool {
//     pattern_word_match(pattern, text, false).is_some()
// }

pub fn match_pattern_all(pattern: &PatternWord, text: &str) -> bool {
    match pattern_word_match(pattern, text, true) {
        Some(MatchResult { start, end }) => start == 0 && end == text.len(),
        None => false,
    }
}

pub fn replace_pattern(
    pattern: &PatternWord,
    text: &str,
    replacement: &str,
    replace_all: bool,
) -> String {
    let regex = pattern.regex(false);
    println!("replace regex {:?}", regex);
    match regex {
        Some(regex) => {
            if replace_all == true {
                regex.replace_all(text, replacement).to_string()
            } else {
                regex.replace(text, replacement).to_string()
            }
        }
        None => {
            // println!("{:?}", err);
            String::new()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn literal_only() {
        let pat = PatternWord::new(
            vec!["abc".to_owned()]
        );

        assert_eq!(
            pattern_word_match(&pat, "abc", true),
            Some(MatchResult { start: 0, end: 3 })
        );

        assert_eq!(
            pattern_word_match(&pat, "abcxx", true),
            None,
        );

        assert_eq!(pattern_word_match(&pat, "", true), None,);

        assert_eq!(pattern_word_match(&pat, "xyz", true), None);

        assert_eq!(
            replace_pattern(&pat, "_abc_abc_abc_", "X", false),
            "_X_abc_abc_".to_owned()
        );

        assert_eq!(
            replace_pattern(&pat, "_abc_abc_abc_", "X", true),
            "_X_X_X_".to_owned()
        );
    }

    #[test]
    fn wildcard() {
        // "?"
        let pat = PatternWord::new(
            vec!["?".to_owned()],
        );

        assert_eq!(pattern_word_match(&pat, "", true), None);

        assert_eq!(
            pattern_word_match(&pat, "@", true),
            Some(MatchResult { start: 0, end: 1 })
        );

        assert_eq!(replace_pattern(&pat, "aaa", "X", false), "Xaa".to_owned());

        assert_eq!(replace_pattern(&pat, "aaa", "X", true), "XXX".to_owned());

        // "*"
        let pat = PatternWord::new(
            vec!["*".to_owned()],
        );

        assert_eq!(pattern_word_match(&pat, "", true), None,);

        assert_eq!(
            pattern_word_match(&pat, "x", true),
            Some(MatchResult { start: 0, end: 1 })
        );

        assert_eq!(
            pattern_word_match(&pat, "xyz", true),
            Some(MatchResult { start: 0, end: 3 })
        );

        // "1?34"
        let pat = PatternWord::new(
            vec!["1?34".to_owned()],
        );

        assert_eq!(
            pattern_word_match(&pat, "abc1234", false),
            Some(MatchResult { start: 3, end: 7 })
        );

        assert_eq!(
            replace_pattern(&pat, "_1A34_1B34_1C34_", "X", false),
            "_X_1B34_1C34_".to_owned()
        );

        assert_eq!(
            replace_pattern(&pat, "_1A34_1B34_1C34_", "X", true),
            "_X_X_X_".to_owned()
        );

        // "1*4"
        let pat = PatternWord::new(
            vec!["1*4".to_owned()],
        );

        assert_eq!(
            pattern_word_match(&pat, "##1234##", false),
            Some(MatchResult { start: 2, end: 6 })
        );
    }

    #[test]
    fn complex_pattern() {
        // "1?3*78*9"
        let pat = PatternWord::new(
            vec!["1?3*78*9".to_owned()],
        );

        assert_eq!(
            pattern_word_match(&pat, "123456789__", false),
            Some(MatchResult { start: 0, end: 9 })
        );

        assert_eq!(pattern_word_match(&pat, "12x3456789__", false), None);
    }
}
