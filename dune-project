(lang dune 2.8)
(name undotter)

(generate_opam_files true)

(source (github inria-parkas/undotter))
(license BSD2)
(authors "Timothy Bourke")
(maintainers "tim@tbrk.org")

(package
  (name undotter)
  (synopsis "Break strongly connected dot files up into smaller chunks")
  (description "Undotter is a tool for visualizing strongly connected components in large graphs. It reads a file in the Graphviz dot format and recursively smashes its strongly connected components into smaller chunks until each contains less than a specified number of nodes (30 by default), it then outputs them in separate dot files. Chunk subgraphs are replaced by a representative node labelled with the name of the corresponding generated file.")
  (depends
    (ocamlgraph (>= 1.8.8))))

