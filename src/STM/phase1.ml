(*go from a gospel TAST into a Drv.t *)

open Ortac_core
module W = Warnings
open Types
open Ppxlib
open Gospel
include Builder
module T = Translation
module Ident = Identifier.Ident
module Ts = Translated
module I = Map.Make (Int)


let string_of_ident = Fmt.str "%a" Ident.pp


let new_name name = let loc = !Ast_helper.default_loc in 
  string_of_ident (Ident.create name ~loc)

let term_printer ?(v = true) _text _global_loc (t : Tterm.term)  =
  if v then () else ();
  Fmt.str "%a" Tterm_printer.print_term t

let _econst = function
  | Pconst_integer (c, o) ->
    Pconst_integer (c, o) |> pexp_constant |> fun e ->
    eapply (evar "Z.of_int") [ e ]
   | _ as e -> pexp_constant e


(*translates a TAST term into a ppx expression.*)
let rec unsafe_term ~old_state_name ~state_arg ~driver (t : Tterm.term)
  : expression =
  let open Fmt in
  let is_state (t : Tterm.term_node) =
    (*is a term EQUAL to a field of the Tvar vsymbol representing the state argument
    returns None if not equal and Some field name otherwise *)
    Option.bind state_arg (fun state_arg -> 
        match t with
        | Tfield (t, field_name) ->
          (match t.t_node with
           | Tvar vs ->
             if Symbols.Vs.compare vs state_arg = 0 then
             Some (string_of_ident field_name.ls_name)
             else  None
           | _ -> None)
        (*start here if there is a bug this equality is wrong*)
        | _ -> None) in
  let term = unsafe_term ~old_state_name ~state_arg ~driver in
  let loc = t.t_loc in
  let unsupported m = raise (W.Error (W.Unsupported m, loc)) in
  match t.t_node with
  | Told t0 -> (match (is_state t0.t_node) with
    | None -> unsupported "old operator used on a term that is not the state (STM)"
    | Some field_name ->  pexp_field (evar old_state_name) (lident field_name))
  | Tvar { vs_name; _ } -> evar (str "%a" Ident.pp vs_name)
  | Tconst c -> pexp_constant c
  | Tfield (t, f) -> pexp_field (term t) (lident f.ls_name.id_str)
 (*below is really brittle but the correct change should happen in gospel.*)
  | Tapp (fs, [t]) when fs.ls_name.id_str = "integer_of_int" -> term t
  | Tapp (fs, []) when Symbols.(ls_equal fs fs_bool_true) -> [%expr true]
  | Tapp (fs, []) when Symbols.(ls_equal fs fs_bool_false) -> [%expr false]
  | Tapp (fs, tlist) when Symbols.is_fs_tuple fs ->
    List.map term tlist |> pexp_tuple
  | Tapp (ls, tlist) when Drv.is_function ls driver ->
    let f = Drv.find_function ls driver in
    eapply (evar f) (List.map term tlist)
  | Tapp (ls, tlist) when Symbols.(ls_equal ls fs_apply) ->
    let f, args =
      match tlist with
      | [] -> assert false
      | x :: xs -> (term x, List.map term xs)
    in
    eapply f args
  | Tapp (ls, tlist) ->
     ( Drv.translate_stdlib ls driver |> function
      | Some f ->
        Printf.eprintf "gospel function being applied is:%s\n%!" f;
        List.fold_left (fun _ s -> Printf.eprintf "attr is:%s\n%!" s) () ls.ls_name.id_attrs; 
        eapply (evar f) (List.map term tlist)
      | None ->
        let func = ls.ls_name.id_str in
        Printf.eprintf "function being applied is:%s\n%!" func;
        List.fold_left (fun _ s -> Printf.eprintf "attr is:%s\n%!" s) () ls.ls_name.id_attrs; 
        if ls.ls_constr then
          (if tlist = [] then None
           else Some (List.map term tlist |> pexp_tuple))
          |> pexp_construct (lident func)
        else eapply (evar func) (List.map term tlist))
          (* kstr unsupported "function application `%s`" func)
             this is brittle because you aren't scoping these fns correctly,
             relying that List.fn is actually Stdlib.List.fn and not Test_module.List.fn,
             but you only need this until gospel expands its stdlib
          *)
  | Tif (i, t, e) -> [%expr if [%e term i] then [%e term t] else [%e term e]]
  | Tlet (x, t1, t2) ->
    let x = str "%a" Ident.pp x.vs_name in
    [%expr
      let [%p pvar x] = [%e term t1] in
      [%e term t2]]
  | Tcase (t, ptl) ->
    List.map
      (fun (p, g, t) ->
         case ~guard:(Option.map term g) ~lhs:(T.pattern p.Tterm.p_node)
           ~rhs:(term t))
      ptl
    |> pexp_match (term t)
  | Tquant (Tterm.Tlambda, args, t) ->
    let t = term t in
    let args =
      List.map
        (fun (vs : Symbols.vsymbol) ->
           (Nolabel, pvar (str "%a" Ident.pp vs.vs_name)))
        args
    in
    efun args t
  | Tquant (Tterm.(Tforall | Texists),  _,  _ ) -> unsupported "ill formed quantification"
  | Tbinop (op, t1, t2) -> (
      match op with
      | Tterm.Tand ->
        let vt1 = gen_symbol ~prefix:"__t1" () in
        let vt2 = gen_symbol ~prefix:"__t2" () in
        [%expr
          let [%p pvar vt1] = [%e term t1] in
          let [%p pvar vt2] = [%e term t2] in
          [%e evar vt1] && [%e evar vt2]]
      | Tterm.Tand_asym -> [%expr [%e term t1] && [%e term t2]]
      | Tterm.Tor ->
        let vt1 = gen_symbol ~prefix:"__t1" () in
        let vt2 = gen_symbol ~prefix:"__t2" () in
        [%expr
          let [%p pvar vt1] = [%e term t1] in
          let [%p pvar vt2] = [%e term t2] in
          [%e evar vt1] || [%e evar vt2]]
      | Tterm.Tor_asym -> [%expr [%e term t1] || [%e term t2]]
      | Tterm.Timplies -> [%expr (not [%e term t1]) || [%e term t2]]
      | Tterm.Tiff -> [%expr [%e term t1] = [%e term t2]])
  | Tnot t -> [%expr not [%e term t]]
  | Ttrue -> [%expr true]
  | Tfalse -> [%expr false]

let term ~old_state_name ~state_arg ~driver (_fail : string) t =
  try Ok (unsafe_term ~old_state_name ~state_arg ~driver t) with
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

 
let with_checks ~old_state_name ~state_arg ~driver (checks: Tterm.term list) (value : Translated.value): Translated.value =
  let txt = "silly" in
  let checks =
    List.map
      (fun t ->
         let loc = t.Tterm.t_loc in 
         let term = term ~old_state_name ~state_arg ~driver "checks" t in
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

let with_pre ~old_state_name ~state_arg ~driver ~term_printer pres (value : Translated.value) =
  let preconditions = List.map (fun t ->
      let txt = term_printer t in
      let loc = t.Tterm.t_loc in
      let translation = term ~old_state_name ~state_arg ~driver "pre " t in 
      ({ txt; loc; translation } : Translated.term)) pres
  in
  { value with preconditions }



let with_post ~old_state_name ~state_arg ~driver ~term_printer (posts : Tterm.term list) (rets: Tast.lb_arg list)
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
      let translation = term  ~old_state_name ~state_arg ~driver "post" t in 
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
let with_xposts  ~old_state_name ~state_arg ~driver (xposts: (Ttypes.xsymbol * (Tterm.pattern * Tterm.term) list) list)
    (value : Translated.value) =
  (*xpost processes one raises into a case list*)
  let xpost ((exn : Ttypes.xsymbol), (ptlist : (Tterm.pattern * Tterm.term) list)) =
    let name : string = exn.Ttypes.xs_ident.id_str in
    let cases =
      List.map
        (fun (p, t) ->
           term  ~old_state_name ~state_arg ~driver "xpost" t
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



let value  ~old_state_name ~state_arg ~driver ~ghost (vd : Tast.val_description): Drv.t   =
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
      |> with_checks  ~old_state_name ~state_arg ~driver spec.sp_checks 
      |> with_pre ~old_state_name ~state_arg ~driver ~term_printer spec.sp_pre
      |> with_post ~old_state_name ~state_arg ~driver ~term_printer spec.sp_post spec.sp_ret
      |> with_xposts ~old_state_name ~state_arg ~driver spec.sp_xpost
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
let signature ~driver s : Drv.t * string =
let state_arg (args: Tast.lb_arg list) = List.find_opt (fun arg ->
      let ty = Tast_helper.ty_of_lb_arg arg in
      (match ty.ty_node with
       | Tyvar tv -> tv.tv_name.id_str = "t" 
       | Tyapp (ty, _) -> ty.ts_ident.id_str = "t")
    ) args 
in
  let old_state_name = new_name "old_state" in 
  (List.fold_left
    (fun driver (sig_item : Tast.signature_item) ->
       match sig_item.sig_desc with
         | Sig_val (vd, ghost) when vd.vd_args <> [] ->
           let state_arg = try Option.map Tast_helper.vs_of_lb_arg (state_arg vd.vd_args)
             with Invalid_argument _ -> None 
           in
           (*start here change this to allow the state to be any of the args*)
           value ~old_state_name ~state_arg ~driver ~ghost vd
       | Sig_val (_, _) -> driver (*ignoring constants*) 
       | Sig_type (_rec, td, ghost) -> types ~driver:driver ~ghost td
       (* | Sig_function func when Option.is_none func.fun_ls.ls_value ->
          predicate ~driver func*)
     (*  | Sig_function func -> function_ ~driver func
         still idk what goes in here
     *)
       (*  | Sig_axiom ax -> axiom ~driver ax *)
       | _ -> driver)
    driver s, old_state_name)
