## RLox a fully fledged lox interpreter in rust

A Rust implementation for the Lox language in [Crafting Interpreters by Robert Nystrom](https://craftinginterpreters.com/).

This is roughly the first interpreter from the book (jlox) not the second.

WIP

## Divergence from the book 
1. Exceptions. Mostly turned into `Result<BLAH, String>` and liberal use of `?` to propagate them up.
2. The visitor pattern is very pretty in c++ and friends, `match` is far more powerful so the visitor pattern is not present at all.
3. I use modules with function-base public interfaces. Eg. scanner (lexer) provides `scanner(file: String) -> Tokens`
   not some class with methods to call. I do mostly use classes as used in the book under the hood.