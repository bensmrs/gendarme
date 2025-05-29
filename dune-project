(lang dune 2.7)
(name marshal)
(version 0.1.0)
(generate_opam_files true)
(license MIT)
(authors "Benjamin Somers <benjamin.somers@imt-atlantique.fr>")
(maintainers "Benjamin Somers <benjamin.somers@imt-atlantique.fr>")
(homepage "https://github.com/bensmrs/marshal")
(source (github "bensmrs/marshal"))
(bug_reports "https://github.com/bensmrs/marshal/issues")

(package
 (name marshal)
 (synopsis "Libraries to marshal OCaml data structures")
 (description "This package provides common primitives to marshal OCaml data structures"))

(package
 (name marshal-json)
 (synopsis "Libraries to marshal OCaml data structures (JSON)")
 (description "This package allows to marshal OCaml data structures to JSON")
 (depends
  (ppx_marshal_ext (= :version))
  (yojson (>= 1.6.0))))

(package
 (name marshal-yaml)
 (synopsis "Libraries to marshal OCaml data structures (YAML)")
 (description "This package allows to marshal OCaml data structures to YAML")
 (depends
  (ppx_marshal_ext (= :version))
  (yaml (>= 3.0.1))))

(package
 (name ppx_marshal)
 (synopsis "Preprocessor extension to marshal OCaml types")
 (description "This package provides a preprocessor extension to automatically define marshallers for OCaml types")
 (depends
  (alcotest :with-test)
  (marshal (= :version))
  (marshal-json (and (= :version) :with-test))
  (marshal-yaml (and (= :version) :with-test))
  (ocaml (>= 4.13.0))
  (ppxlib (and (>= 0.26.0) (< 0.36)))))

(package
 (name ppx_marshal_ext)
 (synopsis "Preprocessor extension to simplify writing Marshal encoders")
 (description "This package provides a preprocessor extension for lazy programmers to save a few lines of code when writing Marshal encoders")
 (depends
  (marshal (= :version))
  (ocaml (>= 4.13.0))
  (ppxlib (and (>= 0.26.0) (< 0.36)))))
