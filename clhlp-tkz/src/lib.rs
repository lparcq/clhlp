//! Command Line Tokenizer
//!
//! Command line parser for interpreters implemented as a space separated commands.
//!
//! Token types depends on the previous tokens and the position in the line.
//! An object implementing trait `Syntax` must be provided to tell the tokenizer what is the
//! expected type of the current token. It also parses dates.

use std::fmt::{self, Display};
use thiserror::Error as ThisError;

pub use chrono::{
    format::{ParseError, ParseResult},
    offset::LocalResult,
    DateTime, FixedOffset, Local, NaiveDate, NaiveDateTime, TimeZone,
};

/// All possible token types.
#[derive(Debug, PartialEq, Eq)]
pub enum TokenType<K> {
    Keyword(K),
    Name,
    Operator,
    String,
    Unsigned,
    Signed,
    Float,
    Date,
}

/// All possible tokens. Keywords are provided as a generic type.
///
/// If `TokenType::String` is expected, the resulting token can be either:
/// - `Token::String` for unquoted, single quoted or double quoted strings.
/// - `Token::BackQuoted` for back-quoted strings.
/// - `Token::PartialString` for unterminated quoted strings.
#[derive(Debug, PartialEq)]
pub enum Token<K: Display> {
    Keyword(K),
    Name(String),
    Operator(char),
    OpenBracket(char),
    CloseBracket(char),
    String(String),
    BackQuoted(String),
    PartialString(String),
    Unsigned(u64),
    Signed(i64),
    Float(f64),
    Date(DateTime<FixedOffset>),
    Unknown(String),
}

impl<K: Display> Display for Token<K> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Keyword(kw) => write!(f, "{}", kw),
            Token::Name(s) | Token::String(s) | Token::Unknown(s) => write!(f, "{}", s),
            Token::Operator(c) | Token::OpenBracket(c) | Token::CloseBracket(c) => {
                write!(f, "{}", c)
            }
            Token::BackQuoted(s) => write!(f, "`{}`", s),
            Token::PartialString(s) => write!(f, "{}…", s),
            Token::Unsigned(i) => write!(f, "{}", i),
            Token::Signed(i) => write!(f, "{}", i),
            Token::Float(i) => write!(f, "{}", i),
            Token::Date(dt) => write!(f, "{}", dt),
        }
    }
}

/// A token with the start and end position in the original string.
#[derive(Debug, PartialEq)]
pub struct TokenSpan<K: Display> {
    pub token: Token<K>,
    pub start: usize,
    pub end: usize,
}

impl<K: Display> TokenSpan<K> {
    fn new(token: Token<K>, start: usize, end: usize) -> Self {
        Self { token, start, end }
    }

    pub fn narrow<'a>(&self, line: &'a str) -> &'a str {
        &line[self.start..self.end]
    }
}

// Create a token from a variant, a value and the position
macro_rules! token_span {
    ( $variant:ident, $value:expr, $start:expr, $end:expr ) => {
        TokenSpan::new(Token::$variant($value), $start, $end)
    };
}

/// Error returned on invalid dates
#[derive(ThisError, Debug)]
pub enum DateError {
    #[error(transparent)]
    Parsing(#[from] ParseError),
    #[error("{0}: invalid local time")]
    InvalidLocalTime(String),
    #[error("{0}: ambiguous local time")]
    AmbiguousLocalTime(String),
}

/// Result of date conversion
pub type DateResult = Result<DateTime<FixedOffset>, DateError>;

/// Convert a naive date time
fn from_naive(naive: &NaiveDateTime, value: &str) -> DateResult {
    match Local.from_local_datetime(naive) {
        LocalResult::None => Err(DateError::InvalidLocalTime(value.to_string())),
        LocalResult::Single(dt) => Ok(DateTime::<FixedOffset>::from(dt)),
        LocalResult::Ambiguous(_, _) => Err(DateError::AmbiguousLocalTime(value.to_string())),
    }
}

/// Parse local date (without time).
fn parse_local_date(value: &str, format: &str) -> DateResult {
    let naive = NaiveDate::parse_from_str(value, format)?;
    match naive.and_hms_opt(0, 0, 0) {
        Some(naive) => from_naive(&naive, value),
        None => Err(DateError::InvalidLocalTime(value.to_string())),
    }
}

/// Parse local date time (with hour).
fn parse_local_datetime(value: &str, format: &str) -> DateResult {
    let naive = NaiveDateTime::parse_from_str(value, format)?;
    from_naive(&naive, value)
}

/// Defines the syntax of lines.
///
/// The only mandatory function to implement is `token_type`.
pub trait Syntax {
    type Keyword: Display;

    /// Parse a date.
    ///
    /// Default implementation parses RFC 3339 and ISO 8601 date and time string
    /// such as 2010-09-08T12:34:56-08:00 or partial dates: 2010-09-08, …
    fn parse_date(&self, value: &str) -> DateResult {
        match value.len() {
            8 => parse_local_date(value, "%y-%m-%d"),
            10 => parse_local_date(value, "%Y-%m-%d"),
            14 => parse_local_datetime(value, "%y-%m-%dT%H:%M"),
            16 => parse_local_datetime(value, "%Y-%m-%dT%H:%M"),
            17 => parse_local_datetime(value, "%y-%m-%dT%H:%M:%S"),
            19 => parse_local_datetime(value, "%Y-%m-%dT%H:%M:%S"),
            _ => DateTime::parse_from_rfc3339(value).map_err(DateError::from),
        }
    }

    /// Return true if current character is an operator.
    ///
    /// Operators are not necessarily separated from other arguments with a space.
    fn is_operator(&self, _spans: &[TokenSpan<Self::Keyword>], _ch: char) -> bool {
        false
    }

    /// Return true if current character is an opening or closing bracket.
    ///
    /// Called for characters like [, ], (, ), {, }. By default, they are all
    /// considered as brackets.
    fn is_bracket(&self, _spans: &[TokenSpan<Self::Keyword>], _ch: char) -> bool {
        true
    }

    /// Define the token type based on previous tokens and current value.
    ///
    /// Most of the time, the token type only depends on the position in the
    /// command line. If the command is `echo`, all remaining arguments are
    /// strings. If the command is `set-date`, the only following argument is
    /// probably a date.
    ///
    /// Sometimes, the token type can also depend on the value.
    fn token_type(
        &self,
        spans: &[TokenSpan<Self::Keyword>],
        value: &str,
    ) -> Option<TokenType<Self::Keyword>>;
}

/// Test is a span matches token types.
///
/// ```rust,no_run
/// # use clhlp_tkz::{span_matches, Token, TokenType, TokenSpan};
/// # let span = TokenSpan::<&'static str>{ token: Token::Name("name".to_string()), start: 0, end: 4 };
/// if span_matches!(span, Name | String) {
/// }
/// ```
#[macro_export]
macro_rules! span_matches {
    ( $span:expr, $variant:ident ) => {
        matches!(
            $span,
            TokenSpan {
                token: Token::$variant(_),
                start: _,
                end: _
            }
        )
    };
    ( $span:expr, $first_variant:ident | $( $variant:ident )|* ) => {
        matches!(
            $span,
            TokenSpan {
                token: Token::$first_variant(_) $( | Token::$variant(_) )*,
                start: _,
                end: _
            }
        )
    };
}

fn append_char(s: &str, ch: char) -> String {
    let mut t = String::from(s);
    t.push(ch);
    t
}

/// Parse command lines similar to shell commands.
///
/// - Strings can be single or double-quoted. Backquotes define a special type of string.
/// - Operators are: !#$%&*/:;<>=?|~^.
/// - Brackets are: (), [] and {}
pub struct Tokenizer<'a, S: Syntax> {
    syntax: &'a S,
    string_opening: char,
    string_closing: char,
}

impl<'a, S: Syntax> Tokenizer<'a, S> {
    pub fn new(syntax: &'a S) -> Self {
        Self {
            syntax,
            string_opening: '\0',
            string_closing: '\0',
        }
    }

    /// Create a new token base on the token type and the current state of the parser.
    ///
    /// Dates are parsed by the provided Syntax trait.
    fn new_token(&self, kind: TokenType<S::Keyword>, value: &str) -> Token<S::Keyword> {
        match kind {
            TokenType::Keyword(kw) => Token::Keyword(kw),
            TokenType::Name => Token::Name(value.to_string()),
            TokenType::String => {
                if self.string_opening != '\0' {
                    Token::PartialString(value.to_string())
                } else if self.string_closing == '`' {
                    Token::BackQuoted(value.to_string())
                } else {
                    Token::String(value.to_string())
                }
            }
            TokenType::Operator => value
                .chars()
                .fold(None, |acc, ch| match acc {
                    None => Some(Token::Operator(ch)),
                    Some(Token::Operator(ch)) => Some(Token::Unknown(String::from(ch))),
                    Some(Token::Unknown(s)) => Some(Token::Unknown(append_char(&s, ch))),
                    _ => panic!("internal error"),
                })
                .expect("internal error"),
            TokenType::Unsigned => value
                .parse::<u64>()
                .map(Token::Unsigned)
                .unwrap_or_else(|_| Token::Unknown(value.to_string())),
            TokenType::Signed => value
                .parse::<i64>()
                .map(Token::Signed)
                .unwrap_or_else(|_| Token::Unknown(value.to_string())),
            TokenType::Float => value
                .parse::<f64>()
                .map(Token::Float)
                .unwrap_or_else(|_| Token::Unknown(value.to_string())),
            TokenType::Date => self
                .syntax
                .parse_date(value)
                .map(Token::Date)
                .unwrap_or_else(|_| Token::Unknown(value.to_string())),
        }
    }

    /// Append a span to the list if the buffer is not only whitespaces.
    fn push_span(
        &self,
        spans: &mut Vec<TokenSpan<S::Keyword>>,
        buffer: &mut String,
        start: usize,
        end: usize,
    ) {
        let value = buffer.trim_start();
        if !value.is_empty() {
            let token = match self.syntax.token_type(spans, value) {
                Some(token_type) => self.new_token(token_type, value),
                None => Token::Unknown(value.to_string()),
            };
            spans.push(TokenSpan::new(token, start, end))
        }
        buffer.clear();
    }

    /// Parse a line and return the list of tokens and their positions in the line.
    pub fn parse(&mut self, line: &str) -> Vec<TokenSpan<S::Keyword>> {
        let mut start = 0;
        let mut escape_next = false;
        let mut buffer = String::new();
        let mut spans = Vec::new();
        self.string_opening = '\0';
        self.string_closing = '\0';
        for (pos, ch) in line.char_indices() {
            if escape_next {
                buffer.push(ch);
                escape_next = false;
            } else if ch == '\\' {
                escape_next = true;
            } else if self.string_opening == '\0' {
                let is_empty = buffer.is_empty();
                match ch {
                    '!' | '#' | '$' | '%' | '&' | '+' | '-' | '*' | '/' | ':' | ';' | '<' | '>'
                    | '=' | '?' | '|' | '~' | '^'
                        if is_empty && self.syntax.is_operator(&spans, ch) =>
                    {
                        spans.push(token_span!(Operator, ch, start, pos + 1));
                        start = pos + 1;
                    }
                    '[' | '(' | '{' if self.syntax.is_bracket(&spans, ch) => {
                        self.push_span(&mut spans, &mut buffer, start, pos);
                        spans.push(token_span!(OpenBracket, ch, pos, pos + 1));
                        start = pos + 1;
                    }
                    ']' | ')' | '}' if self.syntax.is_bracket(&spans, ch) => {
                        self.push_span(&mut spans, &mut buffer, start, pos);
                        spans.push(token_span!(CloseBracket, ch, pos, pos + 1));
                        start = pos + 1;
                    }
                    ' ' => {
                        self.push_span(&mut spans, &mut buffer, start, pos);
                        start = pos + 1;
                    }
                    '"' | '\'' | '`' => self.string_opening = ch,
                    ch => buffer.push(ch),
                }
            } else if self.string_opening == ch {
                self.string_closing = self.string_opening;
                self.string_opening = '\0';
            } else {
                buffer.push(ch);
            }
        }
        self.push_span(&mut spans, &mut buffer, start, line.len());
        spans
    }
}

#[cfg(test)]
mod test {

    use chrono::{DateTime, Local, LocalResult, NaiveDate, TimeZone};
    use std::{fmt, str::FromStr};
    use thiserror::Error as ThisError;

    use super::{parse_local_datetime, DateError, Syntax, Token, TokenSpan, TokenType, Tokenizer};

    #[derive(ThisError, Debug)]
    pub enum SyntaxError {
        #[error("{0}: unknown command")]
        UnknownCommand(String),
    }

    // ============================================================
    // Tests with litterals

    #[derive(Debug, PartialEq)]
    struct Null;

    impl fmt::Display for Null {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            f.write_str("(null)")
        }
    }
    struct SyntaxWithLitterals {}

    impl Syntax for SyntaxWithLitterals {
        type Keyword = Null;

        fn is_operator(&self, spans: &[TokenSpan<Self::Keyword>], ch: char) -> bool {
            match spans.last() {
                None => false,
                Some(span) => {
                    matches!(ch, '+' | '-' | '/' | '*')
                        && span_matches!(span, Signed | Unsigned | Float)
                }
            }
        }

        fn token_type(
            &self,
            _spans: &[TokenSpan<Self::Keyword>],
            value: &str,
        ) -> Option<TokenType<Self::Keyword>> {
            if value.parse::<i64>().is_ok() {
                Some(TokenType::Signed)
            } else {
                Some(TokenType::String)
            }
        }
    }

    macro_rules! assert_local_date {
        ($year:expr, $month:expr, $day:expr, $value: expr) => {
            assert_local_date!($year, $month, $day, 0, 0, 0, $value);
        };
        ($year:expr, $month:expr, $day:expr, $hour:expr, $min:expr, $value: expr) => {
            assert_local_date!($year, $month, $day, $hour, $min, 0, $value);
        };
        ($year:expr, $month:expr, $day:expr, $hour:expr, $min:expr, $sec:expr, $value: expr) => {{
            match Local.from_local_datetime(
                &NaiveDate::from_ymd_opt($year, $month, $day)
                    .expect("invalid date")
                    .and_hms_opt($hour, $min, $sec)
                    .expect("invalid time"),
            ) {
                LocalResult::None => panic!("invalid reference"),
                LocalResult::Single(expected) => assert_eq!(expected, $value),
                LocalResult::Ambiguous(_, _) => panic!("ambiguous reference"),
            }
        }};
    }

    #[test]
    fn test_dates() -> Result<(), DateError> {
        let syntax = SyntaxWithLitterals {};
        assert_local_date!(2022, 2, 12, syntax.parse_date("22-02-12")?);
        assert_local_date!(2022, 2, 12, 14, 41, syntax.parse_date("22-02-12T14:41")?);
        assert_local_date!(
            2022,
            2,
            12,
            14,
            41,
            23,
            syntax.parse_date("22-02-12T14:41:23")?
        );
        assert_local_date!(2022, 2, 12, syntax.parse_date("2022-02-12")?);
        assert_local_date!(2022, 2, 12, 14, 41, syntax.parse_date("2022-02-12T14:41")?);
        assert_local_date!(
            2022,
            2,
            12,
            14,
            41,
            23,
            syntax.parse_date("2022-02-12T14:41:23")?
        );
        assert_eq!(
            DateTime::parse_from_rfc3339("1996-12-19T16:39:57-08:00")?,
            syntax.parse_date("1996-12-19T16:39:57-08:00")?
        );
        Ok(())
    }

    #[test]
    fn test_strings() {
        let syntax = SyntaxWithLitterals {};
        let mut tokenizer = Tokenizer::new(&syntax);

        let expected = vec![
            token_span!(String, "string \"1\"".to_owned(), 0, 14),
            token_span!(String, "string \"2\"".to_owned(), 15, 27),
            token_span!(BackQuoted, "string \"' 3".to_owned(), 28, 41),
        ];
        let line = "\"string \\\"1\\\"\" 'string \"2\"' `string \"' 3`";
        let result = tokenizer.parse(line);
        assert_eq!(&expected[..], &result[..]);
    }

    #[test]
    fn test_operators() {
        let syntax = SyntaxWithLitterals {};
        let mut tokenizer = Tokenizer::new(&syntax);

        for (expected, line) in &[
            (
                vec![
                    token_span!(Signed, -1, 0, 2),
                    token_span!(Operator, '+', 3, 4),
                    token_span!(Signed, 3, 5, 6),
                ],
                "-1 + 3",
            ),
            (
                vec![
                    token_span!(Signed, 1, 0, 1),
                    token_span!(Operator, '-', 2, 3),
                    token_span!(Signed, -3, 3, 5),
                ],
                "1 --3",
            ),
        ] {
            let result = tokenizer.parse(line);
            assert_eq!(&expected[..], &result[..]);
        }
    }

    #[test]
    fn test_brackets() {
        let syntax = SyntaxWithLitterals {};
        let mut tokenizer = Tokenizer::new(&syntax);

        let expected = vec![
            token_span!(String, "one".to_owned(), 0, 3),
            token_span!(OpenBracket, '(', 4, 5),
            token_span!(String, "two".to_owned(), 5, 8),
            token_span!(OpenBracket, '[', 9, 10),
            token_span!(String, "three".to_owned(), 11, 16),
            token_span!(CloseBracket, ']', 17, 18),
            token_span!(OpenBracket, '{', 18, 19),
            token_span!(String, "four".to_owned(), 20, 24),
            token_span!(CloseBracket, '}', 24, 25),
            token_span!(CloseBracket, ')', 25, 26),
        ];
        //          01234567890123456789012345
        let line = "one (two [ three ]{ four})";
        let result = tokenizer.parse(line);
        assert_eq!(&expected[..], &result[..]);
    }

    // ============================================================
    // Tests with keywords

    #[derive(Debug, PartialEq)]
    enum Keyword {
        Help,
        Get,
        Set,
    }

    impl FromStr for Keyword {
        type Err = SyntaxError;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            if s == "help" {
                Ok(Keyword::Help)
            } else if s == "get" {
                Ok(Keyword::Get)
            } else if s == "set" {
                Ok(Keyword::Set)
            } else {
                Err(Self::Err::UnknownCommand(s.to_string()))
            }
        }
    }

    impl fmt::Display for Keyword {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(
                f,
                "{}",
                match self {
                    Keyword::Help => "help",
                    Keyword::Get => "get",
                    Keyword::Set => "set",
                }
            )
        }
    }

    struct SyntaxWithKeywords {}

    impl SyntaxWithKeywords {
        fn keyword(value: &str) -> Option<TokenType<Keyword>> {
            Keyword::from_str(value).ok().map(TokenType::Keyword)
        }
    }

    impl Syntax for SyntaxWithKeywords {
        type Keyword = Keyword;

        fn token_type(
            &self,
            spans: &[TokenSpan<Self::Keyword>],
            value: &str,
        ) -> Option<TokenType<Self::Keyword>> {
            if spans.is_empty() {
                SyntaxWithKeywords::keyword(value)
            } else if spans.len() == 1 {
                match spans[0].token {
                    Token::Keyword(Keyword::Help) => SyntaxWithKeywords::keyword(value),
                    Token::Keyword(Keyword::Get) => Some(TokenType::Name),
                    Token::Keyword(Keyword::Set) => Some(TokenType::Name),
                    _ => None,
                }
            } else if spans.len() == 2 && matches!(spans[0].token, Token::Keyword(Keyword::Set)) {
                let Token::Name(ref name) = spans[1].token else {
                    panic!("expecting a name");
                };
                if name == "date" {
                    Some(TokenType::Date)
                } else {
                    Some(TokenType::String)
                }
            } else {
                None
            }
        }
    }

    #[test]
    fn test_tokenize_line_with_keywords() {
        let syntax = SyntaxWithKeywords {};
        let mut tokenizer = Tokenizer::new(&syntax);

        for (expected, line) in [
            (vec![token_span!(Keyword, Keyword::Help, 0, 4)], "help"),
            (
                vec![
                    token_span!(Keyword, Keyword::Help, 0, 4),
                    token_span!(Keyword, Keyword::Get, 5, 8),
                ],
                "help get",
            ),
            (
                vec![
                    token_span!(Keyword, Keyword::Help, 0, 4),
                    token_span!(Unknown, "unknown".to_owned(), 5, 12),
                ],
                "help unknown ",
            ),
            (
                vec![
                    token_span!(Keyword, Keyword::Get, 0, 3),
                    token_span!(Name, "size".to_owned(), 4, 8),
                ],
                "get size",
            ),
            (
                vec![
                    token_span!(Keyword, Keyword::Get, 0, 3),
                    token_span!(Name, "size".to_owned(), 5, 9),
                ],
                "get  size",
            ),
            (
                vec![
                    token_span!(Keyword, Keyword::Set, 0, 3),
                    token_span!(Name, "mode".to_owned(), 4, 8),
                    token_span!(String, "in \"or\" out".to_owned(), 9, 24),
                ],
                "set mode \"in \\\"or\\\" out\" ",
            ),
            (
                vec![
                    token_span!(Keyword, Keyword::Set, 0, 3),
                    token_span!(Name, "mode".to_owned(), 4, 8),
                    token_span!(PartialString, "in or ".to_owned(), 9, 16),
                ],
                "set mode \"in or ",
            ),
            (
                vec![
                    token_span!(Keyword, Keyword::Set, 0, 3),
                    token_span!(Name, "date".to_owned(), 4, 8),
                    token_span!(
                        Date,
                        parse_local_datetime("2010-09-08T12:34:56", "%Y-%m-%dT%H:%M:%S").unwrap(),
                        9,
                        28
                    ),
                ],
                "set date 2010-09-08T12:34:56",
            ),
        ] {
            let result = tokenizer.parse(line);
            assert_eq!(&expected[..], &result[..]);
            for span in result {
                if let Token::Keyword(ref kw) = span.token {
                    assert_eq!(kw.to_string(), span.narrow(line));
                }
            }
        }
    }
}
