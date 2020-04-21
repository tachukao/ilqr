# iLQR

Implementation of iterative LQR in OCaml.

## Dependencies
1. Owl https://github.com/owlbarn/owl
2. Cmdargs https://github.com/hennequin-lab/cmdargs (only used in examples)

## Installation
```sh
dune build @install && dune install
```

## Run examples
```sh
mkdir results
dune exec examples/pendulum.exe -- -d results
```