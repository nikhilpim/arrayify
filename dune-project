(lang dune 3.17)
(using menhir 3.0)

(name translator)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name <author@example.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name arrayify)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml llvm)
 (tags
  ("add topics" "to describe" your project)))

(package
  (name srkmin)
  (synopsis "Minimal Software Reasoning Kit")
  (license MIT))
; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
