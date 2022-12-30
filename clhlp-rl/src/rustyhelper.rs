use rustyline::{
    completion::{Completer, FilenameCompleter},
    highlight::Highlighter,
    hint::{Hinter, HistoryHinter},
    validate::{ValidationContext, ValidationResult, Validator},
    Context, Result as RustyLineResult,
};
use rustyline_derive::Helper;
use std::{
    borrow::Cow::{self, Borrowed, Owned},
    cell::RefCell,
    fmt::Display,
};

use clhlp_tkz::{span_matches, Syntax, Token, TokenSpan, Tokenizer};

pub use crate::themes::{Style, Theme};

/// Text span with a given style.
struct StyleSpan<'a> {
    pub style: &'a Style,
    pub start: usize,
    pub end: usize,
}

impl<'a> StyleSpan<'a> {
    fn new(style: &'a Style, start: usize, end: usize) -> Self {
        StyleSpan { style, start, end }
    }

    fn with_style(self, style: &'a Style) -> Self {
        StyleSpan::new(style, self.start, self.end)
    }
}

struct HelperState<K: Display> {
    /// Current position in line
    cursor_pos: usize,
    /// Current line length
    line_len: usize,
    /// Last list of tokens
    spans: Vec<TokenSpan<K>>,
}

impl<K: Display> HelperState<K> {
    fn new() -> Self {
        Self {
            cursor_pos: 0,
            line_len: 0,
            spans: Vec::new(),
        }
    }

    fn current_span(&self) -> (Option<usize>, Option<&TokenSpan<K>>) {
        let cursor_pos = self.cursor_pos;
        let current = self
            .spans
            .iter()
            .enumerate()
            .find(|(_, span)| cursor_pos >= span.start && cursor_pos <= span.end);
        (
            current.map(|(scan_pos, _)| scan_pos),
            current.map(|(_, scan)| scan),
        )
    }
}

/// Possible matching bracket.
///
/// - Unbalanced if closing bracket doesn't correspond to the last opening char.
/// - Current if cursor is on the opening bracket corresponding to the current closing bracket.
/// - Previous with the position of the opening bracket corresponding to the closing bracket the cursor is on.
enum BracketMatch {
    Unbalanced,
    Current,
    Previous(usize),
}

/// Find matching and unbalanced brackets.
struct BracketMatcher {
    cursor_pos: usize,
    span_pos: usize,
    stack: Vec<(char, usize, bool)>,
}

impl BracketMatcher {
    fn new(cursor_pos: usize) -> Self {
        Self {
            cursor_pos,
            span_pos: 0,
            stack: Vec::new(),
        }
    }

    /// Check if current span matches a previous one.
    ///
    /// Returns the position in the span list of the matching bracket.
    fn match_span<K: Display>(&mut self, span: &TokenSpan<K>) -> Option<BracketMatch> {
        let span_pos = self.span_pos;
        self.span_pos += 1;
        let is_pointed = self.cursor_pos >= span.start && self.cursor_pos <= span.end;
        if let Token::OpenBracket(ob) = span.token {
            self.stack.push((ob, span_pos, is_pointed));
            None
        } else if let Token::CloseBracket(cb) = span.token {
            let result = match self.stack.last() {
                None => Some(BracketMatch::Unbalanced),
                Some((ob, open_span_pos, open_is_pointed)) => match (ob, cb) {
                    ('(', ')') | ('[', ']') | ('{', '}') => {
                        if *open_is_pointed {
                            Some(BracketMatch::Current)
                        } else if is_pointed {
                            Some(BracketMatch::Previous(*open_span_pos))
                        } else {
                            None
                        }
                    }
                    _ => Some(BracketMatch::Unbalanced),
                },
            };
            if !matches!(result, Some(BracketMatch::Unbalanced)) {
                self.stack.pop();
            }
            result
        } else {
            None
        }
    }
}

/// Types of completion
pub enum Completion {
    None,
    Path,
    Candidates(Vec<String>),
}

/// Guess possible imput for hints and completion.
pub trait Guesser<S: Syntax> {
    /// Return the list of completion for the token at a given position.
    fn complete(
        &self,
        line: &str,
        spans: &[TokenSpan<S::Keyword>],
        span_pos: Option<usize>,
    ) -> Completion;

    /// Return the possible hint for token at a given position.
    fn hint(
        &self,
        line: &str,
        spans: &[TokenSpan<S::Keyword>],
        span_pos: Option<usize>,
    ) -> Option<String>;
}

/// Syntax helper for rustyline.
#[derive(Helper)]
pub struct RustyLineHelper<'a, 'g, S: Syntax, G: Guesser<S>> {
    theme: Theme,
    syntax: &'a S,
    guesser: &'g G,
    no_style: Style,
    matching_bracket_style: Style,
    filename_completer: FilenameCompleter,
    history_hinter: HistoryHinter,
    /// State
    state: RefCell<HelperState<S::Keyword>>,
}

impl<'a, 'g, S: Syntax, G: Guesser<S>> RustyLineHelper<'a, 'g, S, G> {
    pub fn new(theme: Theme, syntax: &'a S, guesser: &'g G) -> Self {
        RustyLineHelper {
            theme,
            syntax,
            guesser,
            no_style: Style::new(),
            matching_bracket_style: theme.bracket.bold(),
            filename_completer: FilenameCompleter::new(),
            history_hinter: HistoryHinter {},
            state: RefCell::new(HelperState::new()),
        }
    }

    fn parse_line(&self, line: &str) {
        let mut state = self.state.borrow_mut();
        if state.line_len != line.len() {
            let mut tokenizer = Tokenizer::new(self.syntax);
            state.spans = tokenizer.parse(line);
            state.line_len = line.len();
        }
    }

    fn token_style(&self, span: &TokenSpan<S::Keyword>) -> StyleSpan {
        let style = match span.token {
            Token::Keyword(_) => &self.theme.keyword,
            Token::Name(_) => &self.theme.name,
            Token::Operator(_) => &self.theme.operator,
            Token::OpenBracket(_) | Token::CloseBracket(_) => &self.theme.bracket,
            Token::String(_) | Token::BackQuoted(_) | Token::Date(_) => &self.theme.string,
            Token::PartialString(_) => &self.no_style,
            Token::Unsigned(_) | Token::Signed(_) | Token::Float(_) => &self.theme.number,
            Token::Unknown(_) => &self.theme.error,
        };
        StyleSpan::new(style, span.start, span.end)
    }

    fn apply_style(&self, line: &str, span: &StyleSpan) -> String {
        format!("{}", span.style.paint(&line[span.start..span.end]))
    }

    /// Highlight the line based on the list of tokens.
    fn highlight_line(
        &self,
        line: &str,
        cursor_pos: usize,
        spans: &[TokenSpan<S::Keyword>],
    ) -> String {
        let line_end = line.len();
        let mut bracket_matcher = BracketMatcher::new(cursor_pos);
        let mut styles: Vec<StyleSpan> = Vec::new();
        let mut last_is_incomplete = false;
        for span in spans {
            let mut style_span = self.token_style(span);
            if let Some(bmatch) = bracket_matcher.match_span(span) {
                match bmatch {
                    BracketMatch::Unbalanced => style_span.style = &self.theme.error,
                    BracketMatch::Current => style_span.style = &self.matching_bracket_style,
                    BracketMatch::Previous(pos) => styles[pos].style = &self.matching_bracket_style,
                }
            }
            last_is_incomplete = matches!(span.token, Token::Unknown(_)) && span.end == line_end;
            styles.push(style_span);
        }
        if last_is_incomplete {
            if let Some(span) = styles.pop() {
                styles.push(span.with_style(&self.no_style))
            }
        }
        let mut result = String::with_capacity(line.len());
        let mut last = 0;
        for span in styles {
            if span.start > last {
                result += &line[last..span.start];
            }
            result += self.apply_style(line, &span).as_str();
            last = span.end;
        }
        if last < line.len() {
            result += &line[last..];
        }
        result
    }
}

impl<'a, 'g, S: Syntax, G: Guesser<S>> Highlighter for RustyLineHelper<'a, 'g, S, G> {
    fn highlight_prompt<'b, 's: 'b, 'p: 'b>(
        &'s self,
        prompt: &'p str,
        default: bool,
    ) -> Cow<'b, str> {
        if default {
            let colored_prompt = format!("{}", self.theme.prompt.paint(prompt));
            Owned(colored_prompt)
        } else {
            Borrowed(prompt)
        }
    }

    fn highlight_hint<'h>(&self, hint: &'h str) -> Cow<'h, str> {
        Owned(format!("{}", self.theme.hint.paint(hint)))
    }

    fn highlight<'l>(&self, line: &'l str, cursor_pos: usize) -> Cow<'l, str> {
        self.parse_line(line);
        let mut state = self.state.borrow_mut();
        state.cursor_pos = cursor_pos;
        Owned(self.highlight_line(line, cursor_pos, &state.spans))
    }

    fn highlight_char(&self, _line: &str, _cursor_pos: usize) -> bool {
        // No optimization yet. If only the position is changed and the cursor is not moving on
        // a bracket or away from a bracket span, the function can return false.
        true
    }
}

impl<'a, 'g, S: Syntax, G: Guesser<S>> Validator for RustyLineHelper<'a, 'g, S, G> {
    fn validate(&self, _ctx: &mut ValidationContext<'_>) -> RustyLineResult<ValidationResult> {
        let state = self.state.borrow();
        let nerrors = state
            .spans
            .iter()
            .filter(|span| span_matches!(span, Unknown))
            .count();
        Ok(if nerrors == 0 {
            ValidationResult::Valid(None)
        } else if nerrors == 1 && span_matches!(state.spans.last().unwrap(), Unknown) {
            ValidationResult::Incomplete
        } else {
            ValidationResult::Invalid(None)
        })
    }

    fn validate_while_typing(&self) -> bool {
        true
    }
}

impl<'a, 'g, S: Syntax, G: Guesser<S>> Completer for RustyLineHelper<'a, 'g, S, G> {
    type Candidate = <FilenameCompleter as Completer>::Candidate;

    /// Complete the token the cursor is on.
    fn complete(
        &self,
        line: &str,
        cursor_pos: usize,
        ctx: &Context<'_>,
    ) -> RustyLineResult<(usize, Vec<Self::Candidate>)> {
        self.parse_line(line);
        let mut state = self.state.borrow_mut();
        state.cursor_pos = cursor_pos;
        let (span_pos, span) = state.current_span();

        match self
            .guesser
            .complete(line, state.spans.as_slice(), span_pos)
        {
            Completion::None => Ok((cursor_pos, Vec::new())),
            Completion::Path => self.filename_completer.complete(line, cursor_pos, ctx),
            Completion::Candidates(candidates) => Ok((
                span.map(|span| span.start).unwrap_or(0),
                candidates
                    .iter()
                    .map(|s| Self::Candidate {
                        display: s.to_string(),
                        replacement: s.to_string(),
                    })
                    .collect::<Vec<Self::Candidate>>(),
            )),
        }
    }
}

impl<'a, 'g, S: Syntax, G: Guesser<S>> Hinter for RustyLineHelper<'a, 'g, S, G> {
    type Hint = <HistoryHinter as Hinter>::Hint;

    fn hint(&self, line: &str, cursor_pos: usize, ctx: &Context<'_>) -> Option<Self::Hint> {
        self.parse_line(line);
        let mut state = self.state.borrow_mut();
        state.cursor_pos = cursor_pos;
        let (span_pos, _) = state.current_span();
        self.guesser
            .hint(line, &state.spans, span_pos)
            .or_else(|| self.history_hinter.hint(line, cursor_pos, ctx))
    }
}

#[cfg(test)]
mod test {

    use std::fmt::Display;

    use crate::tokenizer::{Token, TokenSpan};

    use super::HelperState;

    fn new_span<K: Display>(kw: K, start: usize, end: usize) -> TokenSpan<K> {
        TokenSpan {
            token: Token::Keyword(kw),
            start,
            end,
        }
    }

    macro_rules! match_current_span {
        ( $pair:expr, $pos:expr, $start:expr) => {
            matches!(
                $pair,
                (
                    Some($pos),
                    Some(TokenSpan {
                        token: _,
                        start: $start,
                        end: _
                    })
                )
            )
        };
    }

    #[test]
    fn test_helper_state() {
        let mut hs = HelperState::<&'static str>::new();
        hs.spans = vec![new_span("one", 0, 3), new_span("two", 4, 6)];
        assert!(matches!(
            hs.current_span(),
            (
                Some(0),
                Some(TokenSpan {
                    token: _,
                    start: 0,
                    end: _
                })
            )
        ));
        hs.cursor_pos = 3;
        assert!(match_current_span!(hs.current_span(), 0, 0));
        hs.cursor_pos = 4;
        assert!(match_current_span!(hs.current_span(), 1, 4));
        hs.cursor_pos = 5;
        assert!(match_current_span!(hs.current_span(), 1, 4));
        hs.cursor_pos = 6;
        assert!(match_current_span!(hs.current_span(), 1, 4));
    }
}
