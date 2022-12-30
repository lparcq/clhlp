use argh::FromArgs;

use clhlp_rl::themes::*;

/// Print themes
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

fn print_theme(theme: Theme, name: Option<&str>) {
    if let Some(name) = name {
        print!("{:<10}", name);
    }
    print!("{} ", theme.prompt.paint(format!("{:<8}", "prompt")));
    print!("{} ", theme.hint.paint(format!("{:<8}", "hint")));
    print!("{} ", theme.keyword.paint(format!("{:<8}", "keyword")));
    print!("{} ", theme.name.paint(format!("{:<8}", "name")));
    print!("{} ", theme.comment.paint(format!("{:<8}", "comment")));
    print!("{} ", theme.string.paint(format!("{:<8}", "string")));
    print!("{} ", theme.error.paint(format!("{:<8}", "error")));
    print!("{} ", theme.number.paint(format!("{:<8}", "number")));
    print!("{} ", theme.operator.paint(format!("{:<8}", "operator")));
    println!("{}", theme.bracket.paint("bracket"));
}

fn main() -> rustyline::Result<()> {
    let opt: Opt = argh::from_env();
    match opt.theme {
        Some(theme) => print_theme(theme, None),
        None => {
            print_theme(Theme::black_and_white(), Some("bw"));
            print_theme(Theme::light8(), Some("light8"));
            print_theme(Theme::dark8(), Some("dark8"));
            print_theme(Theme::light256(), Some("light256"));
            print_theme(Theme::dark256(), Some("dark256"));
        }
    }
    Ok(())
}
