# tiger

Implementing a compiler for `tiger` as described in the book **Modern Compiler Implementation in ML** by Andrew Appel.

## Generate lexer
```bash
# Remove extraneous lines (tests dont work otherwise)
ocamllex lib/lexer.mll && sed -i '' "/^#.*$/c\\" lib/lexer.ml
```

This generates `lib/lexer.ml` (this file is typically checked into the repository).

## Generate parser
```bash
ocamlyacc lib/grammar.mly && sed -i '' "/^#.*$/c\\" lib/grammar.ml
# Additional something that we need to add to the mli file
cat lib/grammar.mli_ >> lib/grammar.mli
```

This generates `lib/grammar.ml` (this file is typically checked into the repository).

## Run tests
To run unit tests
```bash
dune runtest
```
