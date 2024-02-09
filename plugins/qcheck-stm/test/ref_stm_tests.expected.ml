[@@@ocaml.warning "-26-27"]
open Ref
module Spec =
  struct
    open STM
    type sut = t
    type cmd =
      | Get 
    let show_cmd cmd__001_ =
      match cmd__001_ with | Get -> Format.asprintf "%s" "get"
    type nonrec state = {
      value: Ortac_runtime.integer }
    let init_state =
      let i_1 = 42 in
      {
        value =
          (try Ortac_runtime.Gospelstdlib.integer_of_int i_1
           with
           | e ->
               raise
                 (Ortac_runtime.Partial_function
                    (e,
                      {
                        Ortac_runtime.start =
                          {
                            pos_fname = "ref.mli";
                            pos_lnum = 6;
                            pos_bol = 211;
                            pos_cnum = 233
                          };
                        Ortac_runtime.stop =
                          {
                            pos_fname = "ref.mli";
                            pos_lnum = 6;
                            pos_bol = 211;
                            pos_cnum = 234
                          }
                      })))
      }
    let init_sut () = make 42
    let cleanup _ = ()
    let arb_cmd _ =
      let open QCheck in
        make ~print:show_cmd (let open Gen in oneof [pure Get])
    let next_state cmd__002_ state__003_ =
      match cmd__002_ with | Get -> state__003_
    let precond cmd__008_ state__009_ = match cmd__008_ with | Get -> true
    let postcond _ _ _ = true
    let run cmd__010_ sut__011_ =
      match cmd__010_ with | Get -> Res (int, (get sut__011_))
  end
module STMTests = (STM_sequential.Make)(Spec)
let wrapped_init_state () = Spec.init_state
let ortac_postcond cmd__004_ state__005_ res__006_ =
  let (++) = Ortac_runtime.(++) in
  let open Spec in
    let open STM in
      let new_state__007_ = lazy (next_state cmd__004_ state__005_) in
      match (cmd__004_, res__006_) with
      | (Get, Res ((Int, _), i)) ->
          if
            (try
               (Ortac_runtime.Gospelstdlib.integer_of_int i) =
                 (Lazy.force new_state__007_).value
             with
             | e ->
                 raise
                   (Ortac_runtime.Partial_function
                      (e,
                        {
                          Ortac_runtime.start =
                            {
                              pos_fname = "ref.mli";
                              pos_lnum = 11;
                              pos_bol = 346;
                              pos_cnum = 358
                            };
                          Ortac_runtime.stop =
                            {
                              pos_fname = "ref.mli";
                              pos_lnum = 11;
                              pos_bol = 346;
                              pos_cnum = 369
                            }
                        })))
          then None
          else
            Some
              ("get",
                [("i = r.value",
                   {
                     Ortac_runtime.start =
                       {
                         pos_fname = "ref.mli";
                         pos_lnum = 11;
                         pos_bol = 346;
                         pos_cnum = 358
                       };
                     Ortac_runtime.stop =
                       {
                         pos_fname = "ref.mli";
                         pos_lnum = 11;
                         pos_bol = 346;
                         pos_cnum = 369
                       }
                   })])
      | _ -> None
let agree_prop cs = let _ = wrapped_init_state () in STMTests.agree_prop cs
let _ =
  let open QCheck in
    QCheck_base_runner.run_tests_main
      (let count = 1000 in
       [Test.make ~count ~name:"Ref STM tests"
          (STMTests.arb_cmds Spec.init_state) agree_prop])
