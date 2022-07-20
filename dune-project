(lang dune 3.1)

(name tiger)

(generate_opam_files true)

(source
 (github jamestjw/tiger))

(authors "James Tan")

(maintainers "James Tan")

(license "BSD-3-Clause")

(package
 (name tiger)
 (synopsis "A compiler for the tiger programming language")
 (description "Implementing a compiler for tiger as described in the book Modern Compiler Implementation in ML by Andrew Appel.")
 (depends ocaml dune stdio base ppx_inline_test ppx_assert ppx_expect)
 (tags (tiger compiler)))
