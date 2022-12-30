Command Line Tokenizer
======================

![Rust](https://github.com/lparcq/clhlp-tkz/workflows/Rust/badge.svg)
[![Crates.io](https://img.shields.io/crates/v/clhlp-tkz.svg)](https://crates.io/crates/clhlp-tkz)
![Crates.io](https://img.shields.io/crates/l/clhlp-tkz)

Command line parser for interpreters implemented as a space separated commands.

For such parsers, token types depends on the previous tokens and the position in the line.
For example if the line is "echo echo" and `echo` is a builtin command, the first occurrence is
a name or a keyword and the second occurence is a string.

Details
-------

- Parse simple and double quoted strings. Backquoted strings have a specific token type.

- A default implementation is provided to parse dates using [chrono](https://docs.rs/chrono/latest/chrono/)
  RFC 3339 and ISO 8601 parsing.

License
-------

Licensed under either of <a href="../LICENSE-APACHE">Apache License, Version
2.0</a> or <a href="../LICENSE-MIT">MIT license</a> at your option.

<br>

<sub>
Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in this crate by you, as defined in the Apache-2.0 license, shall
be dual licensed as above, without any additional terms or conditions.
</sub>
