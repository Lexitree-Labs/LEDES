(lang dune 3.18)

(name ledes)

(generate_opam_files true)

(source
 (github lexitree-labs/LEDES))

(authors "Geoffrey Borough <geoffrey.borough@outlook.com>")

(maintainers "Geoffrey Borough <geoffrey.borough@outlook.com>")

(license MIT)

(documentation https://lexitree-labs.github.io/LEDES/LEDES)

(package
 (name ledes)
 (synopsis "Legal Electronic Data Exchange Standard")
 (description "Legal Electronic Data Exchange Standard")
 (depends
  (ocaml
   (>= 4.08.0))
  (cmdliner
   (>= 1.2.0))
  (angstrom
   (>= 0.16.0))
  ptime
  (re
   (>= 1.7.2))
  (ppx_expect :with-test))
 (tags
  (parser legal)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
