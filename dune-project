(lang dune 3.20)

(name violette)

(generate_opam_files true)

(source
 (github qexat/violette))

(authors "lexa <hello@lexa.qexat.com>")

(maintainers "lexa <hello@lexa.qexat.com>")

(license MIT)

(documentation https://qexat.github.io/violette)

(package
 (name violette)
 (synopsis "A simple functional programming language")
 (description
  "violette provides an implementation of the eponymic functional programming language")
 (depends
  (ocaml
   (>= 5.3)))
 (tags
  (functional "programming language" violette)))
