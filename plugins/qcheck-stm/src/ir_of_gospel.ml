open Gospel
open Tast
open Config
open Ir
module Ident = Identifier.Ident

type failure =
  | Init_is_a_constant
  | Init_has_multiple_sut_arguments
  | Init_has_no_sut_argument
  | Init_return_sut

let flatten =
  let rec aux acc = function
    | [] -> acc
    | Error err :: xs -> aux (err :: acc) xs
    | _ :: xs -> aux acc xs
  in
  aux []

let lb_arg_is_of_type ts = function
  | Lunit -> false
  | Lnone vs | Loptional vs | Lnamed vs | Lghost vs -> (
      match vs.vs_ty.ty_node with
      | Tyapp (ts', _) -> Ttypes.ts_equal ts ts'
      | _ -> false)

let lb_arg_is_not_of_type ty lb_arg = not (lb_arg_is_of_type ty lb_arg)

let rec exactly_one p = function
  | [] -> Error Init_has_no_sut_argument
  | x :: xs when p x ->
      if List.exists p xs then Error Init_has_multiple_sut_arguments else Ok ()
  | _ :: xs -> exactly_one p xs

let is_stmable config vd =
  let test_constant =
    match vd.vd_args with [] -> Error Init_is_a_constant | _ -> Ok ()
  in
  let test_return =
    if List.for_all (lb_arg_is_not_of_type config.sut) vd.vd_ret then Ok ()
    else Error Init_return_sut
  in
  flatten
    [
      test_constant;
      exactly_one (lb_arg_is_of_type config.sut) vd.vd_args;
      test_return;
    ]

let val_desc config vd =
  match is_stmable config vd with
  | [] ->
      let id = vd.vd_name.id_str in
      let ty = vd.vd_type in
      Ok { id; ty }
  | failures -> Error failures

let sig_item config s =
  match s.sig_desc with
  | Sig_val (vd, Nonghost) -> Some (val_desc config vd)
  | _ -> None

let signature config = List.filter_map (sig_item config)
