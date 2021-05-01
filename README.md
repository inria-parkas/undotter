Undotter
========

[Graphviz](https://graphviz.org) is a superb tool for visualizing graphs, 
but for directed graphs with many strongly connected nodes, the output of 
the `dot` and `neato` layout tools is not always readable.

This is where Undotter may help. It reads a `.dot` file and recursively 
smashes the strongly connected components into smaller chunks until each 
contains less than a specified number of nodes (30 by default), it then 
outputs them in separate `.dot` files. Chunk subgraphs are replaced by a 
representative node labelled with the name of the corresponding generated 
file.

The “smashing” algorithm works by identifying _strong articulation points_ 
using the algorithm of
[Italiano, Laura, and Santaroni](https://doi.org/10.1016/j.tcs.2011.11.011) 
and then choosing one based on a local measure of quality.

Undotter also tries to improve readability by grouping edges that have the 
same attributes between any two nodes. Edge labels are ignored during 
grouping and concatenated together it the replacement edge.

In each subgraph, edges coming from or going to _external nodes_ in other 
subgraphs are included by default and shown in gray.

Undotter is written in [OCaml](https://ocaml.org) and based on the 
[ocamlgraph](http://ocamlgraph.lri.fr) library. It was developed in the 
[Inria](https://www.inria.fr/fr) [PARKAS](https://parkas.di.ens.fr) team.
The original motivation was to visualize cycles in the dependency graphs of 
very large 
[Lustre](https://www-verimag.imag.fr/The-Lustre-Programming-Language-and) 
programs.

Use
---

```
undotter graph.dot
```

Options
-------

* `--output-prefix`: sets that path and prefix of the generated `.dot` 
  files.

* `--ideal-size`: recursive smashing stops when a chunk contains this many 
  nodes or less. Note that this number does not take into account the number 
  of external nodes.

* `--group-edges-threshold`: sets the threshold for grouping edges. Set to a 
  very high value to avoid grouping.

* `--no-external-edges`: do not include external nodes and edges.

* `--cluster-external-nodes`: group the external nodes according to the 
  chunk in which they occur.

Building
--------

Using [Opam](https://opam.ocaml.org):

1. `opam install dune dune-build-info ocamlgraph`
2. `make`

