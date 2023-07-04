 # TODO for `qcheck-stm` mode

 - [x] Licence (commit 1c2108)
 - [x] handle xposts [ocaml-gospel/ortac#90](https://github.com/ocaml-gospel/ortac/pull/90)
 - [x] report errors and warnings to user [ocaml-gospel#93](https://github.com/ocaml-gospel93)
 - [x] when checking for values returning sut type check that this is not the
   function called by the user for `init_sut` and don't print / store a warning
   for this one [ocaml-gospel/ortac#94](https://github.com/ocaml-gospel/ortac/pull/94)
 - [ ] redesign warning system (better error messages and hierarchy)
 - [ ] write more tests (negative ones that raises some warnings)
 - [ ] use preconditions and checks clauses to specialize QCheck generators.
    One first step could be to focus on ints (index out of bound being the main example).
 - [x] add translation of gospel functions / predicates / types
 - [ ] organise the generated code in modules.
    This is needed by the previous topic (translate gospel items) as gospel
    typechecker allows to declare a gospel function and an OCaml function with
    the same name:
    ```ocaml
    val bob : int -> int
    (*@ y = bob x *)

    (*@ function bob (x : integer) : integer = x + 1 *)
    ```
    Draft idea:
    ```ocaml
    module UnderTest = struct
        include Lib
    end


    module Ghost = struct (* in a second step *)
        (* translate gospel items *)
    end

    module Spec = struct
        (* generated code with qualified names in the translated terms *)
        include UserConfig
    end
    (* instantiate stm functor, declare and run tests *)
    ```

    Other draft idea:
    ```ocaml
    module UnderTest = struct
        open Lib
        let bob = bob
        let bob_1 i = i + 1
    end

    module Spec = struct
        include struct
            open Lib
            type nonrec sut = char t
        end
        let run = ....
            | .... -> ... let open UnderTest in call

    ```
- [ ] let the user give `init_state` in a module passed as argument in the cli.
  We may have to be careful about checking that the expression given for
  `init_sut` is of type `sut`, or we let the compiler warn the user afterward.

# Ideas for use-cases

- [ ] vocal library (see how the `qcheck-stm` mode handle these specifications)
- [ ] [art-w](https://github.com/art-w/mcavl) This project already uses `qcheck-lin`
- [ ] [mirage/optint](https://github.com/mirage/optint) This is a good use case
  for gospel in itself as it decares a common module signature in an `.ml`
  file. The two difficulties are to make gospel look at signatures in
  implementation files and to specify an interface with multiple implementation
- [ ] `Out_channel` from Stdlib. This will need a gospel ghost type so we'll
  need to figure out how to handle them.
- ok bloomer from NL
