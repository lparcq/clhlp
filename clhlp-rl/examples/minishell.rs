use argh::FromArgs;
use dirs::home_dir;
use glob::glob;
use rustyline::{error::ReadlineError, Cmd, CompletionType, Config, EditMode, Editor, KeyEvent};
use std::{
    env, fmt, fs, io,
    iter::IntoIterator,
    path::{Path, PathBuf},
    process::Command,
    str::FromStr,
};
use thiserror::Error as ThisError;

use clhlp_rl::*;

/// Mini-shell
#[derive(FromArgs, PartialEq, Debug)]
struct Opt {
    #[argh(
        option,
        short = 'T',
        from_str_fn(Theme::from_str),
        description = "theme: bw, light8, dark8, light256, dark256)"
    )]
    theme: Option<Theme>,
}

#[derive(ThisError, Debug)]
pub enum SyntaxError {
    #[error("{0}: unknown command")]
    UnknownCommand(String),
}

const KW_HELP: &str = "help";
const KW_CURRENT_DIR: &str = "pwd";
const KW_CHANGE_DIR: &str = "cd";
const KW_LIST_FILES: &str = "ls";

const BUILTINS: [&str; 4] = [KW_HELP, KW_CURRENT_DIR, KW_CHANGE_DIR, KW_LIST_FILES];

/// One keyword per builtin command
#[derive(Clone, Debug, PartialEq)]
enum Keyword {
    Help,
    CurrentDirectory,
    ChangeDirectory,
    ListFiles,
}

impl Keyword {
    /// Command help message.
    fn help(&self) -> &'static str {
        match self {
            Keyword::Help => "print help on commands",
            Keyword::CurrentDirectory => "print current directory",
            Keyword::ChangeDirectory => "change current directory",
            Keyword::ListFiles => "list files",
        }
    }
}

impl FromStr for Keyword {
    type Err = SyntaxError;

    /// Build keyword from the command string.
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s == KW_HELP {
            Ok(Keyword::Help)
        } else if s == KW_CURRENT_DIR {
            Ok(Keyword::CurrentDirectory)
        } else if s == KW_CHANGE_DIR {
            Ok(Keyword::ChangeDirectory)
        } else if s == KW_LIST_FILES {
            Ok(Keyword::ListFiles)
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
                Keyword::Help => KW_HELP,
                Keyword::CurrentDirectory => KW_CURRENT_DIR,
                Keyword::ChangeDirectory => KW_CHANGE_DIR,
                Keyword::ListFiles => KW_LIST_FILES,
            }
        )
    }
}

/// Implement the mini-shell syntax
struct MiniSyntax {}

impl MiniSyntax {
    /// Create a keyword token type.
    fn keyword(value: &str) -> Option<TokenType<Keyword>> {
        Keyword::from_str(value).ok().map(|k| TokenType::Keyword(k))
    }
}

impl Syntax for MiniSyntax {
    type Keyword = Keyword;

    /// Bracket only for external commands
    fn is_bracket(&self, spans: &[TokenSpan<Self::Keyword>], _ch: char) -> bool {
        match spans.first() {
            None => true,
            Some(span) => matches!(span.token, Token::Name(_)),
        }
    }

    /// Token type base on the command and the position.
    fn token_type(
        &self,
        spans: &[TokenSpan<Self::Keyword>],
        value: &str,
    ) -> Option<TokenType<Self::Keyword>> {
        let nspans = spans.len();
        if spans.is_empty() {
            MiniSyntax::keyword(value).or(Some(TokenType::Name))
        } else {
            match spans[0].token {
                Token::Keyword(Keyword::Help) if nspans == 1 => MiniSyntax::keyword(value),
                Token::Keyword(Keyword::ChangeDirectory | Keyword::ListFiles) if nspans == 1 => {
                    Some(TokenType::String)
                }
                Token::Keyword(Keyword::ListFiles) | Token::Name(_) => Some(TokenType::String),
                _ => None,
            }
        }
    }
}

/// Implement the completion and hints of the mini-shell
struct MiniGuesser {}

impl Guesser<MiniSyntax> for MiniGuesser {
    /// Complete command names in first position or path for `cd` and `ls`.
    fn complete(
        &self,
        line: &str,
        spans: &[TokenSpan<<MiniSyntax as Syntax>::Keyword>],
        span_pos: Option<usize>,
    ) -> Completion {
        match spans.first() {
            None => Completion::None,
            Some(first_span) => match span_pos {
                Some(span_pos) => {
                    if span_pos == 0 {
                        let prefix = &line[first_span.start..first_span.end];
                        let candidates = BUILTINS
                            .iter()
                            .filter_map(|kw| {
                                if kw.starts_with(prefix) {
                                    Some(kw.to_string())
                                } else {
                                    None
                                }
                            })
                            .collect::<Vec<String>>();
                        if candidates.is_empty() {
                            Completion::None
                        } else {
                            Completion::Candidates(candidates)
                        }
                    } else if matches!(
                        first_span.token,
                        Token::Keyword(Keyword::ChangeDirectory | Keyword::ListFiles)
                    ) {
                        Completion::Path
                    } else {
                        Completion::None
                    }
                }
                None => Completion::None,
            },
        }
    }

    /// Return the possible hint for token at a given position.
    fn hint(
        &self,
        _line: &str,
        _spans: &[TokenSpan<<MiniSyntax as Syntax>::Keyword>],
        _span_pos: Option<usize>,
    ) -> Option<String> {
        None
    }
}

// Apply f(i) on a span iterator and map to token references.
macro_rules! apply_tokens {
    ( $v:expr, $f:ident, $i:expr ) => {
        $v.iter().$f($i).map(|s| &s.token)
    };
}

/// Print help on a list of command.
fn command_help<'a, I>(items: I)
where
    I: IntoIterator<Item = &'a Token<Keyword>>,
{
    items.into_iter().for_each(|name| match name {
        Token::Keyword(ref kw) => println!("{}: {}", kw, kw.help()),
        token => eprintln!("{}: unknown command", token),
    })
}

/// Print current directory.
fn command_current_directory() {
    match env::current_dir() {
        Ok(path) => println!("current directory: {}", path.display()),
        Err(err) => eprintln!("{}", err),
    }
}

/// Set the current directory (default is home directory).
fn set_current_dir(path: Option<PathBuf>) {
    let Some(path) = path.or_else(|| {
        home_dir()
    }) else {
        eprintln!("no home directory");
        return;
    };
    if let Err(err) = env::set_current_dir(&path) {
        eprintln!("{}: {}", path.display(), err);
    }
}

/// Change current directory.
fn command_change_directory(directory: Option<&Token<Keyword>>) {
    match directory {
        None => set_current_dir(None),
        Some(Token::String(ref path)) => set_current_dir(Some(PathBuf::from(path))),
        Some(arg) => eprintln!("{}: invalid directory", arg),
    }
}

/// List files in a list of paths.
fn list_files<P: AsRef<Path>>(path: P) -> io::Result<()> {
    let path = path.as_ref();
    if path.is_dir() {
        for entry in fs::read_dir(path)? {
            let entry = entry?;
            let path = entry.path();
            println!("{}", path.display());
        }
    } else {
        path.metadata()?;
        println!("{}", path.display());
    }
    Ok(())
}

/// List files in current directory.
fn list_current_dir() {
    match env::current_dir() {
        Ok(ref path) => {
            list_files(path).unwrap_or_else(|err| eprintln!("{}: {}", path.display(), err))
        }
        Err(err) => eprintln!("{}", err),
    }
}

/// Print the list of files matching the arguments
fn command_list_files<'a, I>(items: I)
where
    I: IntoIterator<Item = &'a Token<Keyword>>,
{
    items.into_iter().for_each(|token| {
        if let Token::String(ref pattern) = token {
            match glob(pattern) {
                Ok(entries) => {
                    let mut count = 0;
                    entries.for_each(|res| {
                        count += 1;
                        match res {
                            Ok(ref path) => list_files(path)
                                .unwrap_or_else(|err| eprintln!("{}: {}", path.display(), err)),
                            Err(err) => eprintln!("{}", err),
                        }
                    });
                    if count == 0 {
                        list_files(pattern).unwrap_or_else(|err| eprintln!("{}: {}", pattern, err));
                    }
                }
                Err(err) => eprintln!("{}: {}", pattern, err),
            }
        } else {
            eprintln!("{:?}: invalid argument", token);
        }
    })
}

/// Execute and external command
fn command_spawn<'a, I>(program: &str, items: I)
where
    I: IntoIterator<Item = &'a Token<Keyword>>,
{
    let proc = Command::new(program)
        .args(items.into_iter().map(Token::to_string))
        .spawn();
    match proc {
        Ok(mut child) => match child.wait() {
            Ok(exit_code) => println!("{}", exit_code),
            Err(err) => eprintln!("{}: {}", program, err),
        },
        Err(err) => eprintln!("{}: {}", program, err),
    }
}

/// Execute the builtin commands.
fn execute_command(tokens: &[TokenSpan<Keyword>]) {
    if let Some(span) = tokens.first() {
        let ntokens = tokens.len();
        match span.token {
            Token::Keyword(Keyword::Help) if ntokens == 1 => {
                let all = vec![
                    &Token::Keyword(Keyword::CurrentDirectory),
                    &Token::Keyword(Keyword::ChangeDirectory),
                    &Token::Keyword(Keyword::ListFiles),
                ];
                command_help(all);
            }
            Token::Keyword(Keyword::Help) => command_help(apply_tokens!(tokens, skip, 1)),
            Token::Keyword(Keyword::CurrentDirectory) => command_current_directory(),
            Token::Keyword(Keyword::ChangeDirectory) => {
                command_change_directory(apply_tokens!(tokens, nth, 1))
            }
            Token::Keyword(Keyword::ListFiles) if ntokens == 1 => list_current_dir(),
            Token::Keyword(Keyword::ListFiles) => {
                command_list_files(apply_tokens!(tokens, skip, 1))
            }
            Token::Name(ref program) => command_spawn(program, apply_tokens!(tokens, skip, 1)),
            _ => eprintln!("{}: unknown command", span.token),
        }
    }
}

fn main() -> rustyline::Result<()> {
    let opt: Opt = argh::from_env();
    let theme = opt.theme.unwrap_or_else(|| Theme::black_and_white());

    let config = Config::builder()
        .completion_type(CompletionType::List)
        .edit_mode(EditMode::Emacs)
        .build();

    #[cfg(all(debug_assertions, target_os = "linux"))]
    unsafe {
        libc::prctl(libc::PR_SET_PTRACER, -1, 0, 0, 0);
    }

    let syntax = MiniSyntax {};
    let guesser = MiniGuesser {};
    let helper = RustyLineHelper::new(theme, &syntax, &guesser);
    let mut tokenizer = Tokenizer::new(&syntax);
    let mut rl = Editor::<RustyLineHelper<MiniSyntax, MiniGuesser>>::with_config(config)?;
    rl.set_helper(Some(helper));
    rl.bind_sequence(KeyEvent::alt('n'), Cmd::HistorySearchForward);
    rl.bind_sequence(KeyEvent::alt('p'), Cmd::HistorySearchBackward);
    let mut count = 1;
    println!("Enter Ctrl-D to quit");
    loop {
        let prompt = format!("{}> ", count);
        let readline = rl.readline(&prompt);
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                let spans = tokenizer.parse(&line);
                execute_command(&spans);
            }
            Err(ReadlineError::Interrupted) => {
                println!("Interrupted");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("Encountered Eof");
                break;
            }
            Err(err) => {
                println!("Error: {err:?}");
                break;
            }
        }
        count += 1;
    }
    Ok(())
}
