 # TODO for `qcheck-stm` mode

 1. Licence DONE
 2. handle xposts DONE (need review)
 3. report non fatal errors DONE
 3. (bis) check that all warnings are stored in reserr (aka write tests)
 3. use preconditions and checks clauses to specialize QCheck generators.
    One first step could be to focus on ints (index out of bound being the main example).
 4. add translation of gospel functions / predicates / types
 5. organise the generated code in modules.
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
