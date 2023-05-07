(lang dune 3.7)

(name Pieuvre)

(generate_opam_files true)

(source
    (github ApollineRodary/Pieuvre))

(authors "Apolline Rodary" "Thibault Hervier")

(package
    (name Pieuvre)
    (description "Proof assistant for the ENS de Lyon functional project course")
    (depends ocaml dune)
)

(using menhir 2.1)