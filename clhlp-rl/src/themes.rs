// Fixed 256 colors
//
//        0       1       2       3       4       5       6       7
//  0  000000  800000  008000  808000  000080  800080  008080  c0c0c0
//  1  808080  ff0000  ffff00  00ff00  0000ff  ff00ff  00ffff  ffffff
//  2  000000  00005f  000087  0000af  0000d7  0000ff  005f00  005f5f
//  3  005f87  005faf  005fd7  005fff  008700  00875f  008787  0087af
//  4  0087d7  0087ff  00af00  00af5f  00af87  00afaf  00afd7  00afff
//  5  00d700  00d75f  00d787  00d7af  00d7d7  00d7ff  00ff00  00ff5f
//  6  00ff87  00ffaf  00ffd7  00ffff  5f0000  5f005f  5f0087  5f00af
//  7  5f00d7  5f00ff  5f5f00  5f5f5f  5f5f87  5f5faf  5f5fd7  5f5fff
//  8  5f8700  5f875f  5f8787  5f87af  5f87d7  5f87ff  5faf00  5faf5f
//  9  5faf87  5fafaf  5fafd7  5fafff  5fd700  5fd75f  5fd787  5fd7af
// 10  5fd7d7  5fd7ff  5fff00  5fff5f  5fff87  5fffaf  5fffd7  5fffff
// 11  870000  87005f  870087  8700af  8700d7  8700ff  875f00  875f5f
// 12  875f87  875faf  875fd7  875fff  878700  87875f  878787  8787af
// 13  8787d7  8787ff  87af00  87af5f  87af87  87afaf  87afd7  87afff
// 14  87d700  87d75f  87d787  87d7af  87d7d7  87d7ff  87ff00  87ff5f
// 15  87ff87  87ffaf  87ffd7  87ffff  af0000  af005f  af0087  af00af
// 16  af00d7  af00ff  af5f00  af5f5f  af5f87  af5faf  af5fd7  af5fff
// 17  af8700  af875f  af8787  af87af  af87d7  af87ff  afaf00  afaf5f
// 18  afaf87  afafaf  afafd7  afafff  afd700  afd75f  afd787  afd7af
// 19  afd7d7  afd7ff  afff00  afff5f  afff87  afffaf  afffd7  afffff
// 20  d70000  d7005f  d70087  d700af  d700d7  d700ff  d75f00  d75f5f
// 21  d75f87  d75faf  d75fd7  d75fff  d78700  d7875f  d78787  d787af
// 22  d787d7  d787ff  d7af00  d7af5f  d7af87  d7afaf  d7afd7  d7afff
// 23  d7d700  d7d75f  d7d787  d7d7af  d7d7d7  d7d7ff  d7ff00  d7ff5f
// 24  d7ff87  d7ffaf  d7ffd7  d7ffff  ff0000  ff005f  ff0087  ff00af
// 25  ff00d7  ff00ff  ff5f00  ff5f5f  ff5f87  ff5faf  ff5fd7  ff5fff
// 26  ff8700  ff875f  ff8787  ff87af  ff87d7  ff87ff  ffaf00  ffaf5f
// 27  ffaf87  ffafaf  ffafd7  ffafff  ffd700  ffd75f  ffd787  ffd7af
// 28  ffd7d7  ffd7ff  ffff00  ffff5f  ffff87  ffffaf  ffffd7  ffffff
// 29  080808  121212  1c1c1c  262626  303030  3a3a3a  444444  4e4e4e
// 30  585858  626262  6c6c6c  767676  808080  8a8a8a  949494  9e9e9e
// 31  a8a8a8  b2b2b2  bcbcbc  c6c6c6  d0d0d0  dadada  e4e4e4  eeeeee

use std::str::FromStr;
use thiserror::Error as ThisError;

pub use ansi_term::{
    Color,
    Colour::{Black, Blue, Cyan, Fixed, Green, Purple, Red, White, Yellow},
    Style,
};

#[derive(ThisError, Debug)]
pub enum ThemeError {
    #[error("unknown theme")]
    UnknownTheme,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Theme {
    /// Style for prompt
    pub prompt: Style,
    /// Style for input hints
    pub hint: Style,
    /// Style for language keywords
    pub keyword: Style,
    /// Style for numbers
    pub number: Style,
    /// Style for strings
    pub string: Style,
    /// Style for names
    pub name: Style,
    /// Style for comments
    pub comment: Style,
    /// Style for operators
    pub operator: Style,
    /// Style for brackets
    pub bracket: Style,
    /// Style for errors
    pub error: Style,
}

impl Theme {
    pub fn black_and_white() -> Self {
        Self {
            prompt: Style::new(),
            hint: Style::new().italic(),
            keyword: Style::new().bold(),
            name: Style::new(),
            comment: Style::new().underline(),
            string: Style::new(),
            error: Style::new().reverse(),
            number: Style::new(),
            operator: Style::new(),
            bracket: Style::new(),
        }
    }

    pub fn light8() -> Self {
        // 0x00 => Color::Black,
        // 0x01 => Color::Red,
        // 0x02 => Color::Green,
        // 0x03 => Color::Yellow,
        // 0x04 => Color::Blue,
        // 0x05 => Color::Purple,
        // 0x06 => Color::Cyan,
        // 0x07 => Color::White,
        Self {
            prompt: Style::new().bold().fg(Blue),
            hint: Style::new().fg(Yellow),
            keyword: Style::new().fg(Purple),
            name: Style::new().fg(Cyan),
            comment: Style::new().italic(),
            string: Style::new().fg(Green),
            error: Style::new().bold().fg(Red),
            number: Style::new().fg(Green),
            operator: Style::new(),
            bracket: Style::new(),
        }
    }

    pub fn dark8() -> Self {
        Theme::light8()
    }

    pub fn light256() -> Self {
        Self {
            prompt: Style::new().bold().fg(Fixed(57)),
            hint: Style::new().fg(Fixed(245)),
            keyword: Style::new().fg(Fixed(21)),
            name: Style::new().fg(Fixed(57)),
            comment: Style::new(),
            string: Style::new().fg(Fixed(130)),
            error: Style::new().bold().fg(Fixed(160)).on(Fixed(252)),
            number: Style::new().fg(Fixed(142)),
            operator: Style::new().fg(Fixed(34)),
            bracket: Style::new().fg(Fixed(168)),
        }
    }

    pub fn dark256() -> Self {
        Self {
            prompt: Style::new().bold().fg(Fixed(75)),
            hint: Style::new().fg(Fixed(240)),
            keyword: Style::new().fg(Fixed(45)),
            name: Style::new().fg(Fixed(69)),
            comment: Style::new(),
            string: Style::new().fg(Fixed(215)),
            error: Style::new().bold().fg(Fixed(196)).on(Fixed(250)),
            number: Style::new().fg(Fixed(143)),
            operator: Style::new().fg(Fixed(46)),
            bracket: Style::new().fg(Fixed(209)),
        }
    }
}

impl FromStr for Theme {
    type Err = ThemeError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s == "bw" {
            Ok(Theme::black_and_white())
        } else if s == "light8" {
            Ok(Theme::light8())
        } else if s == "dark8" {
            Ok(Theme::dark8())
        } else if s == "light256" {
            Ok(Theme::light256())
        } else if s == "dark256" {
            Ok(Theme::dark256())
        } else {
            Err(ThemeError::UnknownTheme)
        }
    }
}
