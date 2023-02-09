1. make ortac-stm accept non-t type as the qcheck-stm sut type
2. choose init-sut from existing functions
3. make ortac-stm skip the unspecified values (and maybe for other reasons)
4. make cli take subcommands rather than options (see with Clément). In order to configure STM subcommand with
    + type t
    + init-sut / init-state (maybe in a ghost value with postconditions)
    + how to instantiate 'a (type variabes) (even give a QCheck.gen/arb)
5. make stm-ortac know about immutabilty of models
6. ortac should know about modifies clauses before branching to STM -- does default need them too?
    - in gospel, `modifies` clause contains a list of term (hopefully, something
      that evaluates to a `Tvar` or a `Tfield`). In the current state, ortac keeps
      the string representation of the term alongside its translation (or why
      it did not manage to translate). Maybe we could also keep the terms
      themselves in order to keep the information from its structure. It could
      be easier to work with than the two other.
