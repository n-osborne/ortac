(*go from a gospel TAST into a Drv.t *)

open Ortac_core
module W = Warnings
open Types

open Ppxlib
open Builder
open Gospel
module T = Translation
module Ident = Identifier.Ident
module Ts = Translated
module I = Map.Make (Int)


let term_printer ?(v = true) _text _global_loc (t : Tterm.term)  =
  if v then () else ();
  Fmt.str "%a" Tterm_printer.print_term t

(*translates a TAST term into a ppx expression.*)
let unsafe_term ~driver (t : Tterm.term) : expression =
  let unsupported m = raise (W.Error (W.Unsupported m, loc)) in
  match t.t_node with
    Tquant (Tterm.(Tforall | Texists),  _,  _ ) -> unsupported "ill formed quantification"
  | _ -> Translation.unsafe_term ~driver:driver t

let term ~driver (_fail : string) t =
  try Ok (unsafe_term ~driver t) with
  W.Error t -> Error t


let type_of_ty ~driver (ty : Ttypes.ty) =
  match ty.ty_node with
  | Tyvar _a -> raise (Failure "polymorphism not supported yet")
  | _ -> Translate.type_of_ty ~driver ty

(*useless right now because gospel -> stm doesn't support type invariants*)
let with_invariants ~_driver _tuple (type_ : Ts.type_)  =
  { type_ with invariants = [] }

let with_models ~driver (fields : (Gospel.Symbols.lsymbol * bool) list)
    (type_: Ts.type_) =
  let has_dup l = let sorted = List.sort String.compare l in
    List.fold_right (fun ele (dup, prev) ->
        ((match prev with
           None -> dup 
        | Some prev -> if (String.equal ele prev) then Some ele else dup ), Some ele)) sorted (None, None)
  |> fst
  in
  let check_dups = List.map (fun (l, _) -> l.Gospel.Symbols.ls_name.id_str) fields |> has_dup  in
  (match check_dups with None -> () | Some dup -> raise (Failure ("duplicate model: " ^ dup)));
  let models = List.map (fun (l, _) -> (l.Gospel.Symbols.ls_name.id_str,
                                        Option.get l.Gospel.Symbols.ls_value
                                       |> Translate.type_of_ty ~driver 
                                       )) fields
      in
      {type_ with models}

let type_ ~(driver : Drv.t) ~ghost (td : Tast.type_declaration) : Drv.t =
  let name = td.td_ts.ts_ident.id_str in
  let loc = td.td_loc in
  let mutable_ = Mutability.type_declaration ~driver td in
  let type_ = Ts.type_ ~name ~loc  ~mutable_ ~ghost in
  (*line above sets all models and invariants to empty*)
  let process ~(type_ : Ts.type_) (spec : Tast.type_spec) =
    let mutable_ = Mutability.(max (type_.Ts.mutable_) (type_spec ~driver spec)) in
    (*mutability is the maximum of the mutability gotten from the driver and the mutability
      in the spec*)
    let (type_ : Ts.type_) =
      type_
      |> with_models ~driver spec.ty_fields
      |> with_invariants ~_driver:driver spec.ty_invariants
      (*need to support invariants, sets to empty for now*)
    in
    { type_ with mutable_ }
  in
  let type_ = Option.fold ~none:type_ ~some:(process ~type_) td.td_spec in
  let type_item : Ts.structure_item = Ts.Type type_ in
   driver |> Drv.add_translation type_item |> Drv.add_type td.td_ts type_
(*type declarations get added to both translation and type lists*)

let types ~driver ~ghost =
  List.fold_left (fun driver -> type_ ~driver ~ghost) driver

 
let with_checks ~driver (checks: Tterm.term list) (value : Translated.value): Translated.value =
  let txt = "silly" in
  let checks =
    List.map
      (fun t ->
         let loc = t.Tterm.t_loc in 
         let term = term ~driver "checks" t in
         let translations =
           Result.map 
              (fun exp -> (exp, exp)
              ) (* because you dont need two checks for
                does raise and doesnt raise invalid_arg
                                       just get the original check content
                   this causes the multiple checks error in ortac
                *)
             term 
         in
         { txt; loc; Translated.translations } )
      checks
  in
  { value with checks }

let with_pre ~driver ~term_printer pres (value : Translated.value) =
  let preconditions = List.map (fun t ->
      let txt = term_printer t in
      let loc = t.Tterm.t_loc in
      let translation = term ~driver "pre " t in 
      ({ txt; loc; translation } : Translated.term)) pres
  in
  { value with preconditions }

(*true if the ident is used as a variable in the term*)
  (*test the let case, tapp case *)


let with_post ~driver ~term_printer (posts : Tterm.term list) (rets: Tast.lb_arg list)
    (value : Translated.value) =
  let contains_rets (t: Tterm.term) : bool =
    let fvs = Tterm_helper.t_free_vars t in
    List.fold_right (fun ret acc ->
        try (let vs = Tast_helper.vs_of_lb_arg ret in
             Symbols.Svs.mem vs fvs || acc)
        with Invalid_argument _ -> acc) rets false in
  let postconditions = List.map (fun t ->
      let txt = term_printer t in
      let loc = t.Tterm.t_loc in
      let translation = term ~driver "post" t in 
      ({ txt; loc; translation } : Translated.term)) posts
  in
  (*add the markings for whether the ensures can be used in next_state*)
  let marks = List.map (fun t -> contains_rets t) posts in (*true if contains ret, false if not*)
  let postconditions  = let open Translated in
    List.map2 (fun post contains_returns -> {post; contains_returns})
      postconditions marks in
  { value with postconditions }


let assert_false_case =
  case ~guard:None ~lhs:[%pat? _] ~rhs:[%expr false]

(*each exception has multiple patterns, terms*)
(*the second element of xposts is passed in as the ptlist in xpost fn below*)
let with_xposts ~driver (xposts: (Ttypes.xsymbol * (Tterm.pattern * Tterm.term) list) list)
    (value : Translated.value) =
  (*xpost processes one raises into a case list*)
  let xpost ((exn : Ttypes.xsymbol), (ptlist : (Tterm.pattern * Tterm.term) list)) =
    let name : string = exn.Ttypes.xs_ident.id_str in
    let cases =
      List.map
        (fun (p, t) ->
          term ~driver "xpost" t
          |> Result.map (fun (t : expression) -> (*turn the term into a case*)
                 case ~guard:None
                   ~lhs:(Translation.xpost_pattern ~driver name p.Tterm.p_node) (*make an xpost pattern*)
                   ~rhs:t
            ))
        (* XXX ptlist must be rev because the cases are given in the
           reverse order by gospel <- false*)
        (List.rev ptlist) (*<- earlier phase ensures that this is
                            never empty even with an exn which is always true*)
    in
    if List.exists Result.is_error cases then
      List.filter_map (function Ok _ -> None | Error x -> Some x) cases
      |> Result.error
    else List.map Result.get_ok cases (*@ [ assert_false_case ]*) |> Result.ok
    (*case list is never empty without the false case and it makes things match with false too early*)
  in
  let xpostconditions : Translated.xpost list = (*turn each tast xpost into a translated xpost*)
    let open Translated in
    List.map
      (fun xp ->
        let xs = fst xp in
        let exn = xs.Ttypes.xs_ident.id_str in
        let args =
          match xs.Ttypes.xs_type with
          | Ttypes.Exn_tuple l -> List.length l
          | Ttypes.Exn_record _ -> 1
        in
        let translation = xpost xp in
        { exn; args; translation })
      xposts
  in
  { value with xpostconditions }



let value ~driver ~ghost (vd : Tast.val_description): Drv.t   =
  let name = vd.vd_name.id_str in
  let loc = vd.vd_loc in
  let register_name = "hoho register name" in
  let arguments = List.map (Translate.var_of_arg ~driver:driver) vd.vd_args in
  (*extracts name, label, and type of the argument. sets modified and consumed to false.
potentially changes the name of args so as not to clash with anything else in scope
    (using the pretty printer for ident)
  *)
  let returns = List.map (Translate.var_of_arg ~driver:driver) vd.vd_ret in
  let pure = false in
  let value =
    Translated.value ~name ~loc ~register_name ~arguments ~returns ~pure ~ghost
      (*sets checks, preconditions, postconditions, xpostconditions to empty*)
  in
  let process ~value (spec : Tast.val_spec) =
  (*  print_endline("sp_text is");
      print_endline(spec.sp_text); *)
    let term_printer = term_printer spec.sp_text spec.sp_loc in
    let value =
      value
      |> with_checks ~driver spec.sp_checks 
      |> with_pre ~driver ~term_printer spec.sp_pre
      |> with_post ~driver ~term_printer spec.sp_post spec.sp_ret
      |> with_xposts ~driver spec.sp_xpost
      (*gospel -> stm does not currently support these
        |> with_consumes spec.sp_cs
        |> with_modified spec.sp_wr *)
    in
    { value with pure = spec.sp_pure }
  in
  let value = Option.fold ~none:value ~some:(process ~value) vd.vd_spec in
  (*process the spec if it exists*)
  let value_item = Translated.Value value in
  let driver =
    if value.pure then
      let ls = Drv.get_ls driver [ name ] in
      Drv.add_function ls name driver
      (* the driver function list contains the functions which can be used in later specifications.
         exclusively pure functions. *)
    else driver
  in let driver = Drv.add_translation value_item driver in driver

(*starts with empty driver (from ortac_core.signature)*)
let signature ~driver s : Drv.t =
  List.fold_left
    (fun driver (sig_item : Tast.signature_item) ->
       match sig_item.sig_desc with
       | Sig_val (vd, ghost) when vd.vd_args <> [] -> value ~driver ~ghost vd
       | Sig_val (_, _) -> driver (*ignoring constants*) 
       | Sig_type (_rec, td, ghost) -> types ~driver:driver ~ghost td
       (* | Sig_function func when Option.is_none func.fun_ls.ls_value ->
          predicate ~driver func*)
     (*  | Sig_function func -> function_ ~driver func
         still idk what goes in here
     *)
       (*  | Sig_axiom ax -> axiom ~driver ax *)
       | _ -> driver)
    driver s
