1. make ortac-stm accept non-t type as the qcheck-stm sut type
2. choose init-sut from existing functions
3. make ortac-stm skip the unspecified values (and maybe for other reasons)
4. make cli take subcommands rather than options (see with Clément). In order to configure STM subcommand with
    + type t
    + init-sut / init-state (maybe in a ghost value with postconditions)
    + how to instantiate 'a (type variabes) (even give a QCheck.gen/arb)
5. make stm-ortac know about immutabilty of models
6. ortac should know about modifies clauses before branching to STM -- does default need them too?
