ocamllex lib/parser/lexer.mll && sed -i '' "/^#.*$/c\\" lib/parser/lexer.ml
ocamlyacc lib/parser/grammar.mly && sed -i '' "/^#.*$/c\\" lib/parser/grammar.ml
cat lib/parser/grammar.mli_ >> lib/parser/grammar.mli
dune build @fmt
dune promote
