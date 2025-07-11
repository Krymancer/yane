(lang dune 3.19)

(name yane)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name <author@example.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name yane)
 (synopsis "Yet Another NES Emulator")
 (description "A Nintendo Entertainment System emulator written in OCaml")
 (depends ocaml dune unix)
 (tags
  ("emulator" "nes" "6502" "retro-gaming")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
