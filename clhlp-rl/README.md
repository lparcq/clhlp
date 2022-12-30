Command Line Rustyline Helpers
==============================

![Rust](https://github.com/lparcq/clhlp-rl/workflows/Rust/badge.svg)
[![Crates.io](https://img.shields.io/crates/v/clhlp-rl.svg)](https://crates.io/crates/clhlp-rl)
![Crates.io](https://img.shields.io/crates/l/clhlp-rl)

[Rustyline](https://docs.rs/rustyline/latest/rustyline/index.html) helpers
for validation and syntax highlighting.

Example
-------

See example _minishell_ in the repository. Most of the logic is provided by implementing the
trait `Syntax`.

Dependency:

```toml
[dependencies]
clhlp-rl = "0.1"
```

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
