include Example_exn
module Ortac_runtime = Ortac_runtime
open QCheck
open STM
type sut = Example_exn.t
type state = {
  contents: Ortac_runtime.Z.t }
type cmd =
  | Fn_one of int [@@deriving show { with_path = false }]
let init_sut = Example_exn.init_sut
let cleanup _ = ()
let arb_cmd _s =
  QCheck.make ~print:show_cmd
    (let open Gen in
       oneof
         [Gen.map (fun arg1 -> Fn_one arg1)
            (frequency [(1, small_nat); (20, int)])])
let next_state c s =
  match c with
  | Fn_one arg1 ->
      let t_arg = s in
      if
        (Ortac_runtime.Z.lt t_arg.contents (Ortac_runtime.Z.of_int 5)) &&
          (Ortac_runtime.Z.gt t_arg.contents (Ortac_runtime.Z.of_int 0))
      then { contents = (Ortac_runtime.Z.of_int 1) }
      else s
let run c sut =
  match c with
  | Fn_one arg1 ->
      Res ((result int exn), (protect (Example_exn.fn_one sut) arg1))
let init_state = { contents = (Ortac_runtime.Z.of_int 0) }
let precond c s =
  match c with
  | Fn_one arg1 ->
      let t_arg = s in
      Ortac_runtime.Z.gt t_arg.contents (Ortac_runtime.Z.of_int 0)
let postcond c s res =
  match (c, res) with
  | (Fn_one arg1, Res ((Result (Int, Exn), _), out)) ->
      let t_arg = s in
      if Ortac_runtime.Z.lt t_arg.contents (Ortac_runtime.Z.of_int 5)
      then
        (match out with
         | Error exn ->
             (match exn with
              | Exn3 _ as exn ->
                  (List.fold_right
                     (fun (truth, matched) ->
                        fun (truth_acc, matched_acc) ->
                          ((truth && truth_acc), (matched || matched_acc)))
                     [(match exn with
                       | Exn3 i_1 ->
                           (((Ortac_runtime.Z.of_int 42) =
                               (Ortac_runtime.Z.of_int 42)), true)
                       | Exn3 _ ->
                           (((Ortac_runtime.Z.of_int 54) =
                               (Ortac_runtime.Z.of_int 54)), true)
                       | _ -> (true, false))] (true, false))
                    |> ((fun (b1, b2) -> b1 && b2))
              | Exn2 _ as exn ->
                  (List.fold_right
                     (fun (truth, matched) ->
                        fun (truth_acc, matched_acc) ->
                          ((truth && truth_acc), (matched || matched_acc)))
                     [(match exn with
                       | Exn2 i -> (true, true)
                       | _ -> (true, false))] (true, false))
                    |> ((fun (b1, b2) -> b1 && b2))
              | Exn1 as exn ->
                  (List.fold_right
                     (fun (truth, matched) ->
                        fun (truth_acc, matched_acc) ->
                          ((truth && truth_acc), (matched || matched_acc)))
                     [(match exn with
                       | Exn1 ->
                           (((Ortac_runtime.Z.of_int arg1) =
                               (Ortac_runtime.Z.of_int 1)), true)
                       | _ -> (true, false));
                     (match exn with
                      | Exn1 ->
                          (((Ortac_runtime.Z.of_int arg1) =
                              (Ortac_runtime.Z.of_int 2)), true)
                      | _ -> (true, false))] (true, false))
                    |> ((fun (b1, b2) -> b1 && b2))
              | Stack_overflow | Out_of_memory -> raise exn
              | _ -> false)
         | Ok out ->
             Ortac_runtime.Z.leq (Ortac_runtime.Z.of_int out)
               (Ortac_runtime.Z.of_int 800))
      else (match out with | Error (Invalid_argument _) -> true | _ -> false)
  | _ -> false
