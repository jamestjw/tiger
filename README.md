# tiger

Implementing a compiler for `tiger` as described in the book **Modern Compiler Implementation in ML** by Andrew Appel. This implementation uses OCaml as opposed to SML as I thought that it'd be easier to pull this off in OCaml.

## What's implemented so far?
1. Lexing
2. Parsing to AST
3. Semantic analysis
4. Production of activation records
5. Translation to intermediate representation (IR)
6. Producing basic blocks and traces from IR
7. Instruction selection (Risc-V)

## Postprocessing
A few steps need to be taken before the code can actually be run (this is only necessary if certain files are edited). All of the below operations are idempotent. A `./run_postprocessing.sh` bash file is included to run all of the below steps.

### Generate lexer
```bash
# Remove extraneous lines (tests dont work otherwise)
ocamllex lib/lexer.mll && sed -i '' "/^#.*$/c\\" lib/lexer.ml
```

This generates `lib/lexer.ml` (this file is typically checked into the repository).

### Generate parser
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
## How to run the compiler
This command compiles an input `tiger` file and produces a Risc-V assembly file.
```bash
dune exec bin/main.exe -- input.tig -o output.s
```

The output file needs to be linked with the runtime library. Here, we use `riscv64-unknown-elf-gcc` as our Risc-V assembler and linker.

```bash
riscv64-unknown-elf-gcc output.s btl/runtime.c -Wl,--wrap=getchar -o ./a.out
```

On a machine that does not run on the Risc-V architecture, we may run the executable using `spike`.

```bash
spike pk ./a.out
```

