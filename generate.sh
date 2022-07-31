ocamllex lib/parser/lexer.mll && sed -i '' "/^#.*$/c\\" lib/parser/lexer.ml
ocamlyacc lib/parser/grammar.mly && sed -i '' "/^#.*$/c\\" lib/parser/grammar.ml
cat lib/parser/grammar-tail.mli_ >> lib/parser/grammar.mli
cat lib/parser/grammer-head.mli_ | cat - lib/parser/grammar.mli > temp && mv temp lib/parser/grammar.mli
dune build @fmt
dune promote
