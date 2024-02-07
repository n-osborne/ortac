open Invariants
module Spec =
  struct
    open STM
    [@@@ocaml.warning "-26-27"]
    type sut = int t
    type cmd =
      | Push of int 
    let show_cmd cmd__001_ =
      match cmd__001_ with
      | Push a_1 -> Format.asprintf "%s %a" "push" (Util.Pp.pp_int true) a_1
    type nonrec state = {
      contents: int list }
    let init_state =
      let a_2 = 42 in
      {
        contents =
          (try a_2 :: []
           with
           | e ->
               raise
                 (Ortac_runtime.Partial_function
                    (e,
                      {
                        Ortac_runtime.start =
                          {
                            pos_fname = "invariants.mli";
                            pos_lnum = 8;
                            pos_bol = 292;
                            pos_cnum = 317
                          };
                        Ortac_runtime.stop =
                          {
                            pos_fname = "invariants.mli";
                            pos_lnum = 8;
                            pos_bol = 292;
                            pos_cnum = 324
                          }
                      })))
      }
    let init_sut () = create 42
    let cleanup _ = ()
    let arb_cmd _ =
      let open QCheck in
        make ~print:show_cmd
          (let open Gen in oneof [(pure (fun a_1 -> Push a_1)) <*> int])
    let next_state cmd__002_ state__003_ =
      match cmd__002_ with
      | Push a_1 ->
          {
            contents =
              ((try a_1 :: state__003_.contents
                with
                | e ->
                    raise
                      (Ortac_runtime.Partial_function
                         (e,
                           {
                             Ortac_runtime.start =
                               {
                                 pos_fname = "invariants.mli";
                                 pos_lnum = 13;
                                 pos_bol = 476;
                                 pos_cnum = 501
                               };
                             Ortac_runtime.stop =
                               {
                                 pos_fname = "invariants.mli";
                                 pos_lnum = 13;
                                 pos_bol = 476;
                                 pos_cnum = 522
                               }
                           }))))
          }
    let precond cmd__008_ state__009_ =
      match cmd__008_ with | Push a_1 -> true
    let postcond cmd__004_ state__005_ res__006_ =
      let new_state__007_ = lazy (next_state cmd__004_ state__005_) in
      match (cmd__004_, res__006_) with
      | (Push a_1, Res ((Unit, _), _)) ->
          (try
             Ortac_runtime.Gospelstdlib.(>)
               (Ortac_runtime.Gospelstdlib.List.length
                  (Lazy.force new_state__007_).contents)
               (Ortac_runtime.Gospelstdlib.integer_of_int 0)
           with
           | e ->
               raise
                 (Ortac_runtime.Partial_function
                    (e,
                      {
                        Ortac_runtime.start =
                          {
                            pos_fname = "invariants.mli";
                            pos_lnum = 4;
                            pos_bol = 110;
                            pos_cnum = 124
                          };
                        Ortac_runtime.stop =
                          {
                            pos_fname = "invariants.mli";
                            pos_lnum = 4;
                            pos_bol = 110;
                            pos_cnum = 150
                          }
                      })))
      | _ -> true
    let run cmd__010_ sut__011_ =
      match cmd__010_ with | Push a_1 -> Res (unit, (push a_1 sut__011_))
  end
module STMTests = (STM_sequential.Make)(Spec)
let wrapped_init_state () =
  let __state__012_ = Spec.init_state in
  if
    try
      Ortac_runtime.Gospelstdlib.(>)
        (Ortac_runtime.Gospelstdlib.List.length __state__012_.contents)
        (Ortac_runtime.Gospelstdlib.integer_of_int 0)
    with
    | e ->
        raise
          (Ortac_runtime.Partial_function
             (e,
               {
                 Ortac_runtime.start =
                   {
                     pos_fname = "invariants.mli";
                     pos_lnum = 4;
                     pos_bol = 110;
                     pos_cnum = 124
                   };
                 Ortac_runtime.stop =
                   {
                     pos_fname = "invariants.mli";
                     pos_lnum = 4;
                     pos_bol = 110;
                     pos_cnum = 150
                   }
               }))
  then __state__012_
  else QCheck.Test.fail_report "INIT_SUT violates type invariants for SUT"
let agree_prop cs = let _ = wrapped_init_state () in STMTests.agree_prop cs
let _ =
  let open QCheck in
    QCheck_base_runner.run_tests_main
      (let count = 1000 in
       [Test.make ~count ~name:"Invariants STM tests"
          (STMTests.arb_cmds Spec.init_state) agree_prop])
