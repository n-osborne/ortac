open Gospel
open Tast
open Config
open Ir
module Ident = Identifier.Ident

let lb_arg_is_of_type ts = function
  | Lunit -> false
  | Lnone vs | Loptional vs | Lnamed vs | Lghost vs -> (
      match vs.vs_ty.ty_node with
      | Tyapp (ts', _) -> Ttypes.ts_equal ts ts'
      | _ -> false)

let lb_arg_is_not_of_type ty lb_arg = not (lb_arg_is_of_type ty lb_arg)

let rec exactly_one p = function
  | [] -> false
  | x :: xs when p x -> List.exists p xs |> not
  | _ :: xs -> exactly_one p xs

let is_stmable config vd =
  (* the value is not a constant *)
  match vd.vd_args with
  | [] -> false
  | args ->
      (* there is exactly one argument of the type sut *)
      exactly_one (lb_arg_is_of_type config.sut) args
      (* the function does not return a value of the type sut *)
      && List.for_all (lb_arg_is_not_of_type config.sut) vd.vd_ret

let val_desc config vd =
  if is_stmable config vd then Some { id = vd.vd_name.id_str } else None

let sig_item config s =
  match s.sig_desc with
  | Sig_val (vd, Nonghost) -> val_desc config vd
  | _ -> None

let signature config = List.filter_map (sig_item config)
