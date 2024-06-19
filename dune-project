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
 (description
  "Implementing a compiler for tiger as described in the book Modern Compiler Implementation in ML by Andrew Appel.")
 (depends
  ocaml
  core_unix
  dune
  stdio
  base
  core
  ppx_jane
  ppx_show
  ppx_deriving
  expect_test_helpers_core
  batteries)
 (tags
  (tiger compiler)))
