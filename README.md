# tiger

Implementing a compiler for `tiger` as described in the book **Modern Compiler Implementation in ML** by Andrew Appel.

## Generate lexer
```bash
ocamllex lib/lexer/lexer.mll
```

This generates `lib/lexer/lexer.ml` (this file is typically checked into the repository).

## Run tests
To run unit tests
```bash
dune runtest
```
