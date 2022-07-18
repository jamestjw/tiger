# tiger

Implementing a compiler for `tiger` as described in the book **Modern Compiler Implementation in ML** by Andrew Appel.

## Generate lexer
```bash
# Remove extraneous lines (tests dont work otherwise)
ocamllex lib/parser/lexer.mll && sed -i '' "/^#.*$/c\\" lib/parser/lexer.ml
```

This generates `lib/parser/lexer.ml` (this file is typically checked into the repository).

## Generate parser
```bash
ocamlyacc lib/parser/grammar.mly && sed -i '' "/^#.*$/c\\" lib/parser/grammar.ml
# Additional something that we need to add to the mli file
cat lib/parser/grammar.mli_ >> lib/parser/grammar.mli
```

This generates `lib/parser/grammar.ml` (this file is typically checked into the repository).

## Run tests
To run unit tests
```bash
dune runtest
```
