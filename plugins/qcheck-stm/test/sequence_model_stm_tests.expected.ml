open Sequence_model
module Spec =
  struct
    open STM
    [@@@ocaml.warning "-26-27"]
    type sut = char t
    type cmd =
      | Add of char 
    let show_cmd cmd__001_ =
      match cmd__001_ with
      | Add v -> Format.asprintf "%s %a" "add" (Util.Pp.pp_char true) v
    type nonrec state = {
      contents: char sequence }
    let init_state =
      let () = () in { contents = Ortac_runtime.Gospelstdlib.Sequence.empty }
    let init_sut () = create ()
    let cleanup _ = ()
    let arb_cmd _ =
      let open QCheck in
        make ~print:show_cmd
          (let open Gen in oneof [(pure (fun v -> Add v)) <*> char])
    let next_state cmd__002_ state__003_ =
      match cmd__002_ with
      | Add v ->
          {
            contents =
              (Ortac_runtime.Gospelstdlib.Sequence.cons v
                 state__003_.contents)
          }
    let precond cmd__008_ state__009_ = match cmd__008_ with | Add v -> true
    let postcond cmd__004_ state__005_ res__006_ =
      let new_state__007_ = lazy (next_state cmd__004_ state__005_) in
      match (cmd__004_, res__006_) with
      | (Add v, Res ((Unit, _), _)) -> true
      | _ -> true
    let run cmd__010_ sut__011_ =
      match cmd__010_ with | Add v -> Res (unit, (add v sut__011_))
  end
module STMTests = (STM_sequential.Make)(Spec)
let _ =
  QCheck_base_runner.run_tests_main
    (let count = 1000 in
     [STMTests.agree_test ~count ~name:"STM Lib test sequential"])
