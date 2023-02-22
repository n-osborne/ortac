open Ppxlib
open Ortac_core.Builder
open Gospel
module Ident = Identifier.Ident
open Tterm
open Tast
open Symbols

(* XXX Hypothesis:
    In the following file, we make the assumption that we can run
    Ortac_core.unsafe_term translation in terms independantly of what we want
    to do with the terms and how we organize them in an intermediary
    representation
  *)

(* should help to find:
  - arguments corresponding to the state
  - modifies clauses that helps computing next_state
*)
type config =
  { sut_ty : Ttypes.ty
  ; init   : string (* to be compared with an Ident.t though *)
  ; models : lsymbol list (* get them in the sut type declaration's specifications *)
  ; driver : Ortac_core.Drv.t
  }

type xpost = Ttypes.xsymbol * (pattern * term) list

type value =
  { value_name : Ident.t    (* this will help building the cmd type    *)
  (* XXX do we really need Ident.t and not just a strin ? *)
  ; value_type : core_type  (* this will help building cmd and arb_cmd *)
  (* we may be better off using gospel types symbols *)
  (* ; value_type : Ttypes.ty  (1* this will help building cmd and arb_cmd *1) *)
  ; args       : Symbols.vsymbol list (* XXX need to remove units and ghosts *)
  ; precond    : term list  (* this will help building precond         *)
  ; postcond   : term list  (* this will help building postcond        *)
  ; xpost      : xpost list (* this will help building postcond        *)
  ; checks     : term list  (* this will help building postcond & next_state *)
  ; next_state : (lsymbol * term) list  (* this will help building next_state      *)
  ; modifies   : term list  (* this will help building next_state, not sure it is useful though *)
  }

type ('a, 'b) predicate = config -> 'a -> 'b option

(* XXX TODO : implement the following functions wisely
    Although, these should not be concerned about the possibility of the gospel
    to OCaml translation, that's a job for ortac_core *)
let choose_precond    : (term, term) predicate = fun _ _ -> None
let choose_postcond   : (term, term) predicate = fun _ _ -> None
(* let choose_next_state : term predicate = fun _ _ -> false *)

(* XXX TODO : this function should inspect a postcondition, decide whether it
   describes a part of a next state and, if so, extract the symbol of the field
   it is describing along side the description *)
(* This is where we could / will plug in a rewrite system to be a bit more
   flexible about how to write specifications *)
let choose_next_state : (term, (lsymbol * term)) predicate =
  fun _ _ -> None

(* remove the unit and the ghosts, return the vsymbol
   may have no use for the config though *)
let choose_args : (lb_arg, Symbols.vsymbol) predicate = fun _ _ -> None

(* XXX TODO : implement a warning system *)
let value config value_name value_type sp =
  let filter p   = List.filter_map (p config) in
  let args       = filter choose_args sp.sp_args in
  let precond    = filter choose_precond sp.sp_pre in
  let postcond   = filter choose_postcond sp.sp_post in
  let xpost      = sp.sp_xpost in
  let checks     = sp.sp_checks in
  let next_state = filter choose_next_state sp.sp_post in
  let modifies   = sp.sp_wr in
  { value_name; value_type; args; precond; postcond; xpost; checks; next_state; modifies }

let lb_arg_is_of_type ty = function
  | Lunit -> false
  | Lnone vs | Loptional vs | Lnamed vs | Lghost vs -> Ttypes.ty_equal vs.vs_ty ty

let lb_arg_is_not_of_type ty lb_arg = not (lb_arg_is_of_type ty lb_arg)

let is_stmable : (val_description, val_spec) predicate =
  fun config vd ->
    if
      (* the value is not the initializing function *)
      vd.vd_name.id_str <> config.init
      (* the value is not a constant *)
      && List.length vd.vd_args > 0
      (* there is exactly one argument of the type of the sut_ty *)
      && List.(filter (lb_arg_is_of_type config.sut_ty) vd.vd_args |> length |> (=) 1)
      (* the function does not return a value of the type of the sut *)
      && List.for_all (lb_arg_is_not_of_type config.sut_ty) vd.vd_ret
    then vd.vd_spec
    else None

let value config vd : value option =
  is_stmable config vd |>  Option.map ( value config vd.vd_name vd.vd_type)

(* XXX TODO beware right associative -- be smart, use an internal tail recursive function *)
let flatten_arrow : core_type -> core_type list = fun _ -> []

let cmd_constructor value =
  let name = noloc value.value_name.id_str in
  let args = Pcstr_tuple (flatten_arrow value.value_type) in
  let res = None in
  constructor_declaration ~name ~args ~res

let cmd values =
  let name =  noloc "cmd" in
  let params = [] in
  let cstrs = [] in
  let kind = Ptype_variant (List.map cmd_constructor values) in
  let private_ = Ppxlib.Public in
  let manifest = None in
  (* XXX TODO add the show attribute *)
  pstr_type Recursive [ type_declaration ~name ~params ~cstrs ~kind ~private_ ~manifest ]

let next_state_case config value : Ppxlib.pattern * expression =
  let args_pat = match value.args with
    | [] -> None
    | xs -> List.map (fun vs -> pvar (Fmt.str "%a" Ident.pp vs.vs_name)) xs
              |> ppat_tuple
              |> Option.some
  in
  let cmd_pat = ppat_construct (lident value.value_name.id_str) args_pat in
  let field_name ls = lident ls.ls_name.id_str in
  let field_value ls = match List.assoc_opt ls value.next_state with
    | None -> assert false (* XXX TODO we should have access to the name of the
    state argument of the next_state function here so that we can reach to the
    non modified field value XXX ot we can postpone if we return a [(longindent
    * expression option) list] with [None] when there is no update *)
    | Some term -> Ortac_core.Translation.unsafe_term ~driver:config.driver term
  in
  let field ls = field_name ls, field_value ls in
  let fields = List.map field config.models
  in
  cmd_pat, pexp_record fields None


