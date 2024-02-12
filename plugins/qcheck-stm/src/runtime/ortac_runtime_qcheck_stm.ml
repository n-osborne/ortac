open STM
include Ortac_runtime

let ( ++ ) a b =
  match (a, b) with
  | None, None -> None
  | Some (_, _), None -> a
  | None, Some (_, _) -> b
  | Some (cmd0, terms0), Some (cmd1, terms1) ->
      assert (cmd0 = cmd1);
      Some (cmd0, terms0 @ terms1)

module Make (Spec : Spec) = struct
  open QCheck
  module Internal = Internal.Make (Spec) [@alert "-internal"]

  let pp_trace ppf trace =
    let open Fmt in
    let pp_aux ppf (c, r) = pf ppf "%s : %s" (Spec.show_cmd c) (show_res r) in
    pf ppf "@[%a@]" (list ~sep:(any "@\n") pp_aux) trace

  let pp_terms ppf err =
    let open Fmt in
    let pp_aux ppf (term, l) = pf ppf "@[%a@\n  @[%s@]@]@\n" pp_loc l term in
    pf ppf "%a" (list ~sep:(any "@\n") pp_aux) err

  let message trace cmd terms =
    Test.fail_reportf
      "Gospel specification violation in function %s\n\
       @;\
      \  @[%a@]@\n\
       when executing the following sequence of operations:@\n\
       @;\
      \  @[%a@]@." cmd pp_terms terms pp_trace trace

  let rec check_disagree postcond s sut cs =
    match cs with
    | [] -> None
    | c :: cs -> (
        let res = Spec.run c sut in
        (* This functor will be called after a modified postcond has been
           defined, returning a list of 3-plets containing the command, the
           term and the location *)
        match postcond c s res with
        | None -> (
            let s' = Spec.next_state c s in
            match check_disagree postcond s' sut cs with
            | None -> None
            | Some (rest, cmd, terms) -> Some ((c, res) :: rest, cmd, terms))
        | Some (cmd, terms) -> Some ([ (c, res) ], cmd, terms))

  let agree_prop wrapped_init_state postcond cs =
    let _ = wrapped_init_state () in
    assume (Internal.cmds_ok Spec.init_state cs);
    let sut = Spec.init_sut () in
    (* reset system's state *)
    let res =
      try Ok (check_disagree postcond Spec.init_state sut cs)
      with exn -> Error exn
    in
    let () = Spec.cleanup sut in
    let res = match res with Ok res -> res | Error exn -> raise exn in
    match res with
    | None -> true
    | Some (trace, cmd, terms) -> message trace cmd terms
  (* Test.fail_reportf *)
  (*   "%a@\n@[@,  when executing this sequence of operations:@]@\n@\n%a@." *)
  (*   pp_err err pp_trace trace *)

  let agree_test ~count ~name wrapped_init_state postcond =
    Test.make ~name ~count
      (Internal.arb_cmds Spec.init_state)
      (agree_prop wrapped_init_state postcond)
end
