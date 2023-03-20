# `qcheck-stm` plugin action plan

Development will be done
[here](https://github.com/n-osborne/tree/qcheck-stm-ortac)

Once we are happy with the plugin, it will be merged
[there](https://github.com/ocaml-gospel/tree/trunk) for demonstration purpose
and review before merging it in main.

Generate a `qcheck-stm` module from `gospel` specifications.
This is based on the work of Naomi Spargo:
[here](https://github.com/naomiiiiiiiii/stm_ortac)

We have modified `ortac` so that it

1. uses a plugin architecture
   [ocaml-gospel#69](https://github.com/ocaml-gospel/ortac/pull/69)
2. cleanly separate the ocaml-of-gospel translation from what is done with it
   [ocaml-gospel#72](https://github.com/ocaml-gospel/ortac/pull/72)

This allows us to:

1. use the same translation process independently of what is build with the
   result
2. in the specific case of the `qcheck-stm` mode, manipulate Gospel terms
   rather than OCaml expressions when building the module structure (for
   example, choosing which postconditions are fit to express the next-state
   function)

We also plan to reuse the last intermediate representation and the code
generation based on it from the original version (see `stm-ortac:src/STM/ast3.ml`)

We will add another intermediate representation, so we will have (names are not
stable yet, they are even quite bad tbh):

`gospel` typed ast → ir + config → ast3 → `qcheck-stm` module

## CONFIG and its initialisation

The config contains:

- the translation context from `ortac.core`
- the type of the system under test given by the user
- the name of the `init_sut` function given by the user

It is initialized at the beginning.

There is already a minimal implementation of the initialisation function.

What remains to do is:

1. [ ] allow for the `init_sut` function to be expressed in a `gospel` attribute
  (*ie* as a ghost function)
2. [ ] allow for the `init_sut` function to be a function call (*eg* `make 16 0`)
3. [ ] search for `sut` and `init` in one pass over the ast
4. [ ] go through the entire signature to deal with `gospel` function and
  predicates: translate them, bind them to an OCaml identifier and store these
  information in the translation context
5. [ ] deal with OCaml values that are annotated with the `pure` `gospel`
  keyword and treat them the same way as `gospel` function

## First pass: ir-of-gospel

The first pass takes the `gospel` typed ast and build an IR.

This intermediate representation's raison d'être is to choose the `gospel`
terms that are fit for an `qcheck-stm` purpose.

The IR has the same list structure as the `signature` in the gospel ast, but:

- it will filter out every values the behaviour of which we don't have enough
  information about to test
- it will reorganize the specifications of these chosen values, having in mind
  the `qcheck-stm` module we want to build

Note that it will still contains `gospel` terms, we postpone their translation
so that it is easier to get information from them. (This is also where we will
plug a rewrite system in order to understand more specification styles.)

For each values we can test, we will need to know:

6.  [ ] how to build the constructor correpsonding to this function in the
    `cmd` type (taht is an identifier and the type of the arguments)
7.  [ ] how to generate a value using this constructor (QCheck)
8.  [ ] how to build the corresponding case of the `next_state` function (a
    subset of the postconditions)
9.  [ ] how to build the corresponding case of the `run` function
10. [ ] how to build the corresponding case of the `precond` function
11. [ ] how to build the corresponding case of the `postcond` function

Note that [6-9] are necessary to have a plugin running (though arguably badly)
by implementing `precond` and `postcond` to always return `true`. [10,11] add
expressivity (and correction / real meaning to the tests).

I expect all these tasks to be possible with the minimal implementation of the
initialisation function describe above. Hence, it should be possible to do
[1-5] and [6-11] in parallel.

## Second pass: ast3-of-ir

The second pass takes an IR and build an ast3 from which we already know how to
build the `qcheck-stm` module. This implies two actions:

12. [ ] dispatch the information linked to a value in IR in the corresponding
    place in `Ast3.stm`
13. [ ] translate the Gospel terms into OCaml expression

These two tasks are expected to be done simultaneously. Though, we should wait
to have a relatively stable IR before beginning to work on this.

This could lead to already tackle the next task:

## Cleanup of ast3

Initially, `ast3` has been implemented when dealing with what is now
`ortac.default` internal representation. This can be cleaned up a bit.
