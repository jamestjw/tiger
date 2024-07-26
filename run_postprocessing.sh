ocamllex lib/lexer.mll && sed -i '' "/^#.*$/c\\" lib/lexer.ml
ocamlyacc lib/grammar.mly && sed -i '' "/^#.*$/c\\" lib/grammar.ml && sed -i '' "/^#.*$/c\\" lib/grammar.mli
cat lib/grammar-tail.mli_ >> lib/grammar.mli
cat lib/grammer-head.mli_ | cat - lib/grammar.mli > temp && mv temp lib/grammar.mli
dune build @fmt
dune promote
