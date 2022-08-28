# `ortac` - OCaml Runtime Assertion Checking.

**Disclamer:** This project is still experimental. No support will be provided
at this point, and its behaviour is still unstable.

## Installation

```
opam pin add -y https://github.com/ocaml-gospel/gospel.git
opam pin add -y https://github.com/ocaml-gospel/ortac.git
opam install ortac
```

## STM Tests

### Create an STM test
Start on OCaml 4.14.0.

If ``path/api.mli`` is a file annotated with Gospel specifications, running 
```
~/stm_ortac/bin >> dune build 
~/stm_ortac/bin >> dune exec -- ./cli.exe --frontend=STM path/api.mli > outpath/run_test.ml
```
will fill the file ``outpath/run_test.ml`` with a module that tests the functions specified in ``path/api.mli``. 

To run the tests in ``run_test.ml``, do 
```
~/outpath >> opam exec --switch=5.1.0+trunk -- dune build
~/outpath >> opam exec --switch=5.1.0+trunk -- dune exec ./run_test.exe
```
To satisfy dependencies (and suppress useless warnings), the dune file in ``outpath`` must be of the form:
```
(executable
 (name run_test)
(preprocess (pps ppx_deriving.show ))
 (libraries multicorecheck.stm qcheck-core qcheck zarith)
)
(env
  (dev
    (flags (:standard -w -26 -w -27))))
```
