We are very happy to announce the initial release of `ortac-core`,
ortac-runtime` and `ortac-qcheck-stm`.

Ortac is a runtime assertion checking tool for OCaml based on Gospel
specification language.

`ortac-core` package exposes a library to translate a subset of Gospel
specification language into OCaml terms. It also provides the `ortac`
command-line tool. You will need to have one of Ortac plugins installed to use
the `ortac` command-line tool.

`ortac-runtime` provides runtime environment that the translated terms depend
on. In particular, it provides an implementation of the executable subset of
the Gospel standard library.

`ortac-qcheck-stm` provides a plugin for Ortac. It generates QCheck-STM
sequential tests for a module specified with Gospel. QCheck-STM is a
model-based testing framework and Ortac/QCheck-STM relies on the Gospel models
you gave in the specifications to build the QCheck-STM tests. See the
[documentation](https://ocaml-gospel.github.io/ortac/ortac-qcheck-stm/index.html)
for more details on how to write the specifications and how to ues the tool to
test your libraries.

