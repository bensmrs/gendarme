(lang dune 2.7)
(name gendarme)
(version 0.3.0)
(generate_opam_files true)
(license MIT)
(authors "Benjamin Somers <benjamin.somers@imt-atlantique.fr>")
(maintainers "Benjamin Somers <benjamin.somers@imt-atlantique.fr>")
(homepage "https://github.com/bensmrs/gendarme")
(source (github "bensmrs/gendarme"))
(bug_reports "https://github.com/bensmrs/gendarme/issues")

(package
 (name gendarme)
 (synopsis "Libraries to marshal OCaml data structures")
 (description "This package provides common primitives to marshal OCaml data structures"))

(package
 (name gendarme-csv)
 (synopsis "Libraries to marshal OCaml data structures (CSV)")
 (description "This package allows to marshal OCaml data structures to CSV")
 (depends
  (ppx_marshal_ext (= :version))
  (csv (>= 2.3))))

(package
 (name gendarme-ezjsonm)
 (synopsis "Libraries to marshal OCaml data structures (JSON [Ezjsonm])")
 (description "This package allows to marshal OCaml data structures to JSON using Ezjsonm")
 (depends
  (ppx_marshal_ext (= :version))
  (ezjsonm (>= 1.1.0))))

(package
 (name gendarme-json)
 (synopsis "Libraries to marshal OCaml data structures (JSON metapackage)")
 (description "Metapackage for JSON marshalling using Gendarme"))

(package
 (name gendarme-toml)
 (synopsis "Libraries to marshal OCaml data structures (TOML)")
 (description "This package allows to marshal OCaml data structures to TOML")
 (depends
  (ppx_marshal_ext (= :version))
  (toml (>= 7.1.0))))

(package
 (name gendarme-yaml)
 (synopsis "Libraries to marshal OCaml data structures (YAML)")
 (description "This package allows to marshal OCaml data structures to YAML")
 (depends
  (ppx_marshal_ext (= :version))
  (yaml (>= 3.0.1))))

(package
 (name gendarme-yojson)
 (synopsis "Libraries to marshal OCaml data structures (JSON [Yojson])")
 (description "This package allows to marshal OCaml data structures to JSON using Yojson")
 (depends
  (ppx_marshal_ext (= :version))
  (yojson (>= 1.6.0))))

(package
 (name ppx_marshal)
 (synopsis "Preprocessor extension to marshal OCaml types")
 (description "This package provides a preprocessor extension to automatically define marshallers for OCaml types")
 (depends
  (alcotest :with-test)
  (gendarme (= :version))
  (gendarme-ezjsonm (and (= :version) :with-test))
  (gendarme-toml (and (= :version) :with-test))
  (gendarme-yaml (and (= :version) :with-test))
  (gendarme-yojson (and (= :version) :with-test))
  (ocaml (>= 4.13.0))
  (ppxlib (and (>= 0.26.0) (< 0.36)))))

(package
 (name ppx_marshal_ext)
 (synopsis "Preprocessor extension to simplify writing Gendarme encoders")
 (description "This package provides a preprocessor extension for lazy programmers to save a few lines of code when writing Gendarme encoders")
 (depends
  (gendarme (= :version))
  (ocaml (>= 4.13.0))
  (ppxlib (and (>= 0.26.0) (< 0.36)))))
