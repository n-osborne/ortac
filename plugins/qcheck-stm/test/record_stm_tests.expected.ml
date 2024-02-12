[@@@ocaml.warning "-26-27"]
open Record
module Ortac_runtime = Ortac_runtime_qcheck_stm
let plus1_1 i =
  try
    Ortac_runtime.Gospelstdlib.(+) i
      (Ortac_runtime.Gospelstdlib.integer_of_int 1)
  with
  | e ->
      raise
        (Ortac_runtime.Partial_function
           (e,
             {
               Ortac_runtime.start =
                 {
                   pos_fname = "record.mli";
                   pos_lnum = 13;
                   pos_bol = 525;
                   pos_cnum = 571
                 };
               Ortac_runtime.stop =
                 {
                   pos_fname = "record.mli";
                   pos_lnum = 13;
                   pos_bol = 525;
                   pos_cnum = 572
                 }
             }))
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
      let i_2 = 42 in
      {
        value =
          (try Ortac_runtime.Gospelstdlib.integer_of_int i_2
           with
           | e ->
               raise
                 (Ortac_runtime.Partial_function
                    (e,
                      {
                        Ortac_runtime.start =
                          {
                            pos_fname = "record.mli";
                            pos_lnum = 7;
                            pos_bol = 285;
                            pos_cnum = 307
                          };
                        Ortac_runtime.stop =
                          {
                            pos_fname = "record.mli";
                            pos_lnum = 7;
                            pos_bol = 285;
                            pos_cnum = 308
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
module STMTests = (Ortac_runtime.Make)(Spec)
let wrapped_init_state () =
  let __state__012_ = Spec.init_state in
  if true
  then __state__012_
  else QCheck.Test.fail_report "INIT_SUT violates type invariants for SUT"
let ortac_postcond cmd__004_ state__005_ res__006_ =
  let (++) = Ortac_runtime.(++) in
  let open Spec in
    let open STM in
      let new_state__007_ = lazy (next_state cmd__004_ state__005_) in
      match (cmd__004_, res__006_) with
      | (Get, Res ((Int, _), i_1)) ->
          (if
             try
               (Ortac_runtime.Gospelstdlib.integer_of_int i_1) =
                 (Lazy.force new_state__007_).value
             with
             | e ->
                 raise
                   (Ortac_runtime.Partial_function
                      (e,
                        {
                          Ortac_runtime.start =
                            {
                              pos_fname = "record.mli";
                              pos_lnum = 23;
                              pos_bol = 868;
                              pos_cnum = 880
                            };
                          Ortac_runtime.stop =
                            {
                              pos_fname = "record.mli";
                              pos_lnum = 23;
                              pos_bol = 868;
                              pos_cnum = 891
                            }
                        }))
           then None
           else
             Some
               ("get",
                 [("i = r.value",
                    {
                      Ortac_runtime.start =
                        {
                          pos_fname = "record.mli";
                          pos_lnum = 23;
                          pos_bol = 868;
                          pos_cnum = 880
                        };
                      Ortac_runtime.stop =
                        {
                          pos_fname = "record.mli";
                          pos_lnum = 23;
                          pos_bol = 868;
                          pos_cnum = 891
                        }
                    })]))
            ++
            ((if
                try
                  (plus1_1 (Ortac_runtime.Gospelstdlib.integer_of_int i_1)) =
                    (Ortac_runtime.Gospelstdlib.(+)
                       (Ortac_runtime.Gospelstdlib.integer_of_int i_1)
                       (Ortac_runtime.Gospelstdlib.integer_of_int 1))
                with
                | e ->
                    raise
                      (Ortac_runtime.Partial_function
                         (e,
                           {
                             Ortac_runtime.start =
                               {
                                 pos_fname = "record.mli";
                                 pos_lnum = 25;
                                 pos_bol = 912;
                                 pos_cnum = 924
                               };
                             Ortac_runtime.stop =
                               {
                                 pos_fname = "record.mli";
                                 pos_lnum = 25;
                                 pos_bol = 912;
                                 pos_cnum = 939
                               }
                           }))
              then None
              else
                Some
                  ("get",
                    [("plus1 i = i + 1",
                       {
                         Ortac_runtime.start =
                           {
                             pos_fname = "record.mli";
                             pos_lnum = 25;
                             pos_bol = 912;
                             pos_cnum = 924
                           };
                         Ortac_runtime.stop =
                           {
                             pos_fname = "record.mli";
                             pos_lnum = 25;
                             pos_bol = 912;
                             pos_cnum = 939
                           }
                       })]))
               ++
               (if
                  (try
                     (Ortac_runtime.Gospelstdlib.integer_of_int (plus2 i_1))
                       =
                       (Ortac_runtime.Gospelstdlib.(+)
                          (Ortac_runtime.Gospelstdlib.integer_of_int i_1)
                          (Ortac_runtime.Gospelstdlib.integer_of_int 2))
                   with
                   | e ->
                       raise
                         (Ortac_runtime.Partial_function
                            (e,
                              {
                                Ortac_runtime.start =
                                  {
                                    pos_fname = "record.mli";
                                    pos_lnum = 26;
                                    pos_bol = 940;
                                    pos_cnum = 952
                                  };
                                Ortac_runtime.stop =
                                  {
                                    pos_fname = "record.mli";
                                    pos_lnum = 26;
                                    pos_bol = 940;
                                    pos_cnum = 967
                                  }
                              })))
                then None
                else
                  Some
                    ("get",
                      [("plus2 i = i + 2",
                         {
                           Ortac_runtime.start =
                             {
                               pos_fname = "record.mli";
                               pos_lnum = 26;
                               pos_bol = 940;
                               pos_cnum = 952
                             };
                           Ortac_runtime.stop =
                             {
                               pos_fname = "record.mli";
                               pos_lnum = 26;
                               pos_bol = 940;
                               pos_cnum = 967
                             }
                         })])))
      | _ -> None
let _ =
  QCheck_base_runner.run_tests_main
    (let count = 1000 in
     [STMTests.agree_test ~count ~name:"Record STM tests" wrapped_init_state
        ortac_postcond])
