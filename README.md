 # TODO for `qcheck-stm` mode

 - [x] Licence (commit 1c2108)
 - [ ] handle xposts #90
 - [ ] report errors and warnings to user #93
 - [ ] write more tests (negative ones that raises some warnings)
 - [ ] use preconditions and checks clauses to specialize QCheck generators.
    One first step could be to focus on ints (index out of bound being the main example).
 - [ ] add translation of gospel functions / predicates / types
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
        type 'a t = 'a Lib.t = def
        let bob = Lib.bob
        let bob_1 i = i + 1
    end

    module Spec = struct
        type sut = char UnderTest.t
        let run = ....
            | .... -> ... let open UnderTest in call

    ```

# Ideas for use-cases

- [ ] vocal library (see how the `qcheck-stm` mode handle these specifications)
- [ ] [art-w](https://github.com/art-w/mcavl) This project already uses `qcheck-lin`
- [ ] [mirage/optint](https://github.com/mirage/optint) This is a good use case for gospel in itself as it decares a common module signature in an `.ml` file. The two difficulties are to make gospel look at signatures in implementation files and to specify an interface with multiple implementation
