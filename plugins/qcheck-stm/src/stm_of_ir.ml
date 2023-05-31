module Cfg = Config
open Ir
open Ppxlib
open Ortac_core.Builder

let ty_default = Ptyp_constr (noloc (Lident "char"), [])
let pat_default = ppat_construct (lident "Char") None
let exp_default = evar "char"

let show_attribute : attribute =
  {
    attr_name = noloc "deriving";
    attr_payload = PStr [ [%stri show { with_path = false }] ];
    attr_loc = Location.none;
  }

let subst_core_type inst ty =
  let rec aux ty =
    {
      ty with
      ptyp_desc =
        (match ty.ptyp_desc with
        | Ptyp_any -> Ptyp_any
        | Ptyp_var x ->
            Option.fold ~none:ty_default
              ~some:(fun x -> x.ptyp_desc)
              (List.assoc_opt x inst)
        | Ptyp_arrow (x, l, r) ->
            let l = aux l and r = aux r in
            Ptyp_arrow (x, l, r)
        | Ptyp_tuple elems ->
            let elems = List.map aux elems in
            Ptyp_tuple elems
        | Ptyp_constr (c, args) ->
            let args = List.map aux args in
            Ptyp_constr (c, args)
        | Ptyp_object (_, _)
        | Ptyp_class (_, _)
        | Ptyp_alias (_, _)
        | Ptyp_variant (_, _, _)
        | Ptyp_poly (_, _)
        | Ptyp_package _ | Ptyp_extension _ ->
            failwith "Case should not happen in `subst'");
    }
  in
  aux ty

let lazy_force =
  let open Gospel in
  let open Tterm_helper in
  let vs_name = Ident.create ~loc:Location.none "Lazy.force"
  and vs_ty = Ttypes.fresh_ty_var "a" in
  let lazy_force = mk_term (Tvar { vs_name; vs_ty }) None Location.none in
  fun t ->
    Tterm_helper.(
      mk_term (Tapp (Symbols.fs_apply, [ lazy_force; t ])) None Location.none)

let ocaml_of_term cfg t =
  let open Ortac_core.Ocaml_of_gospel in
  let open Reserr in
  try term ~context:cfg.Cfg.context t |> ok with W.Error e -> error e

let subst_term ~gos_t ?(old_lz = false) ~old_t ?(new_lz = false) ~new_t term =
  let exception ImpossibleSubst of (Gospel.Tterm.term * [ `New | `Old ]) in
  let rec aux cur_lz cur_t term =
    let open Gospel.Tterm in
    let next = aux cur_lz cur_t in
    match term.t_node with
    | Tconst _ -> term
    | Tvar { vs_name; vs_ty } when vs_name = gos_t -> (
        match cur_t with
        | Some cur_t ->
            let t = { term with t_node = Tvar { vs_name = cur_t; vs_ty } } in
            if cur_lz then lazy_force t else t
        | None ->
            raise (ImpossibleSubst (term, if cur_t = new_t then `New else `Old))
        )
    | Tvar _ -> term
    | Tapp (ls, terms) -> { term with t_node = Tapp (ls, List.map next terms) }
    | Tfield (t, ls) -> { term with t_node = Tfield (next t, ls) }
    | Tif (cnd, thn, els) ->
        { term with t_node = Tif (next cnd, next thn, next els) }
    | Tlet (vs, t1, t2) -> { term with t_node = Tlet (vs, next t1, next t2) }
    | Tcase (t, brchs) ->
        {
          term with
          t_node =
            Tcase
              ( next t,
                List.map
                  (fun (p, ot, t) -> (p, Option.map next ot, next t))
                  brchs );
        }
    | Tquant (q, vs, t) -> { term with t_node = Tquant (q, vs, next t) }
    | Tbinop (o, l, r) -> { term with t_node = Tbinop (o, next l, next r) }
    | Tnot t -> { term with t_node = Tnot (next t) }
    | Told t -> aux old_lz old_t t
    | Ttrue -> term
    | Tfalse -> term
  in
  let open Reserr in
  try ok (aux new_lz new_t term)
  with ImpossibleSubst (t, b) ->
    error
      ( Impossible_term_substitution
          (Fmt.str "%a" Gospel.Tterm_printer.print_term t, b),
        t.t_loc )

let str_of_ident = Fmt.str "%a" Gospel.Identifier.Ident.pp

let mk_cmd_pattern value =
  let pat_args = function
    | _, None -> punit
    | _, Some x -> ppat_var (noloc (str_of_ident x))
  in
  let args =
    match value.args with
    | [] -> None
    | [ x ] -> Some (pat_args x)
    | xs -> List.map pat_args xs |> ppat_tuple |> Option.some
  in
  let name = String.capitalize_ascii (str_of_ident value.id) |> lident in
  ppat_construct name args

let munge_longident cap ty lid =
  let open Reserr in
  match lid.txt with
  | Lident i | Ldot (Lident i, "t") | Ldot (Ldot (_, i), "t") | Ldot (_, i) ->
      let f =
        if cap then String.capitalize_ascii else String.uncapitalize_ascii
      in
      ok (f i)
  | Lapply (_, _) ->
      error
        (Type_not_supported (Fmt.str "%a" Pprintast.core_type ty), ty.ptyp_loc)

let pat_of_core_type inst typ =
  let rec aux ty =
    let open Reserr in
    match ty.ptyp_desc with
    | Ptyp_var v -> (
        match List.assoc_opt v inst with
        | None -> ok pat_default
        | Some t -> aux t)
    | Ptyp_constr (c, xs) ->
        let constr_str = lident <$> munge_longident true ty c
        and pat_arg =
          match xs with
          | [] -> ok None
          | xs -> (fun xs -> Some (ppat_tuple xs)) <$> map aux xs
        in
        ppat_construct <$> constr_str <*> pat_arg
    | _ ->
        error
          ( Type_not_supported (Fmt.str "%a" Pprintast.core_type typ),
            typ.ptyp_loc )
  in
  aux typ

let exp_of_core_type inst typ =
  let rec aux ty =
    let open Reserr in
    match ty.ptyp_desc with
    | Ptyp_var v -> (
        match List.assoc_opt v inst with
        | None -> ok exp_default
        | Some t -> aux t)
    | Ptyp_constr (c, xs) -> (
        let constr_str = evar <$> munge_longident false ty c in
        match xs with
        | [] -> constr_str
        | xs ->
            pexp_apply
            <$> constr_str
            <*> (List.map (fun e -> (Nolabel, e)) <$> map aux xs))
    | _ ->
        error
          ( Type_not_supported (Fmt.str "%a" Pprintast.core_type typ),
            typ.ptyp_loc )
  in
  aux typ

let exp_of_ident id = pexp_ident (lident (str_of_ident id))

let arb_cmd_case value =
  let open Reserr in
  let epure = pexp_ident (lident "pure") in
  let pure e = pexp_apply epure [ (Nolabel, e) ] in
  let fun_cstr =
    let args =
      List.map
        (function
          | _, None -> (Nolabel, punit)
          | _, Some id -> (Nolabel, ppat_var (noloc (str_of_ident id))))
        value.args
    in
    let name = String.capitalize_ascii (str_of_ident value.id) |> lident in
    let body =
      pexp_construct name
        (pexp_tuple_opt
           (List.map
              (function
                | _, None -> eunit | _, Some id -> evar (str_of_ident id))
              value.args))
    in
    efun args body |> pure
  in
  let* gen_args =
    (* XXX TODO: use `requires` clauses to build smarter generators *)
    map (fun (ty, _) -> exp_of_core_type value.inst ty) value.args
  in
  let app l r = pexp_apply (evar "( <*> )") [ (Nolabel, l); (Nolabel, r) ] in
  List.fold_left app fun_cstr gen_args |> ok

let arb_cmd ir =
  let open Reserr in
  let* cmds = elist <$> map arb_cmd_case ir.values in
  let open Ppxlib in
  let let_open str e =
    pexp_open Ast_helper.(Opn.mk (Mod.ident (lident str |> noloc))) e
  in
  let oneof = let_open "Gen" cmds in
  let body =
    let_open "QCheck"
      (pexp_apply (evar "make")
         [ (Labelled "print", evar "show_cmd"); (Nolabel, oneof) ])
  in
  let pat = pvar "arb_cmd" in
  let expr = efun [ (Nolabel, ppat_any (* for now we don't use it *)) ] body in
  pstr_value Nonrecursive [ value_binding ~pat ~expr ] |> ok

let run_case config sut_name value =
  let lhs = mk_cmd_pattern value in
  let open Reserr in
  let* rhs =
    let res = lident "Res" in
    let* ty_show = exp_of_core_type value.inst (Ir.get_return_type value) in
    (* XXX TODO protect iff there are exceptional postconditions or checks *)
    (* let call = function_call_exp (evar sut_name) value in *)
    let call =
      let efun = exp_of_ident value.id in
      let mk_arg = Option.fold ~none:eunit ~some:exp_of_ident in
      let rec aux ty args =
        match (ty.ptyp_desc, args) with
        | Ptyp_arrow (lb, l, r), xs when Cfg.is_sut config l ->
            (lb, evar sut_name) :: aux r xs
        | Ptyp_arrow (lb, _, r), x :: xs -> (lb, mk_arg x) :: aux r xs
        | _, [] -> []
        | _, _ ->
            failwith
              "shouldn't happen (list of arguments should be consistent with \
               type)"
      in
      pexp_apply efun (aux value.ty (List.map snd value.args))
    in
    let args = Some (pexp_tuple [ ty_show; call ]) in
    pexp_construct res args |> ok
  in
  case ~lhs ~guard:None ~rhs |> ok

let run config ir =
  let cmd_name = gen_symbol ~prefix:"cmd" () in
  let sut_name = gen_symbol ~prefix:"sut" () in
  let open Reserr in
  let* cases = map (run_case config sut_name) ir.values in
  let body = pexp_match (evar cmd_name) cases in
  let pat = pvar "run" in
  let expr = efun [ (Nolabel, pvar cmd_name); (Nolabel, pvar sut_name) ] body in
  pstr_value Nonrecursive [ value_binding ~pat ~expr ] |> ok

let next_state_case config state_ident value =
  let state_var = str_of_ident state_ident |> evar in
  let lhs = mk_cmd_pattern value in
  let open Reserr in
  let* idx, rhs =
    (* substitute state variable when under `old` operator and translate description into ocaml *)
    let descriptions =
      List.filter_map
        (fun (i, { model; description }) ->
          subst_term ~gos_t:value.sut_var ~old_t:(Some state_ident) ~new_t:None
            description
          >>= ocaml_of_term config
          |> to_option
          |> Option.map (fun description -> (i, model, description)))
        value.next_state.formulae
    in
    (* choose one and only one description per modified model *)
    let pick id =
      List.find_opt
        (fun (_, m, _) -> Gospel.Identifier.Ident.equal id m)
        descriptions
    in
    let* descriptions =
      map
        (fun id ->
          of_option
            ~default:
              (Ensures_not_found_for_next_state (str_of_ident id), id.id_loc)
            (pick id))
        value.next_state.modifies
    in
    (* [idx], like [descriptions], is in the order of the modifies clauses *)
    let idx = List.map (fun (i, _, _) -> i) descriptions in
    match
      List.map (fun (_, m, e) -> (lident (str_of_ident m), e)) descriptions
    with
    | [] -> ok (idx, state_var)
    | fields ->
        (idx, pexp_record fields (Some (evar (str_of_ident state_ident)))) |> ok
  in
  (idx, case ~lhs ~guard:None ~rhs) |> ok

let next_state config ir =
  let cmd_name = gen_symbol ~prefix:"cmd" () in
  let state_name = gen_symbol ~prefix:"state" () in
  let state_ident = Gospel.Tast.Ident.create ~loc:Location.none state_name in
  let open Reserr in
  let* idx_cases =
    map
      (fun v ->
        let* i, c = next_state_case config state_ident v in
        ok ((v.id, i), c))
      ir.values
  in
  let idx, cases = List.split idx_cases in
  let body = pexp_match (evar cmd_name) cases in
  let pat = pvar "next_state" in
  let expr =
    efun [ (Nolabel, pvar cmd_name); (Nolabel, pvar state_name) ] body
  in
  (idx, pstr_value Nonrecursive [ value_binding ~pat ~expr ]) |> ok

let may_raise_exception v =
  match (v.postcond.exceptional, v.postcond.checks) with
  | [], [] -> false
  | _, _ -> true

let postcond_case config idx state_ident new_state_ident value =
  let idx = List.sort Int.compare idx in
  let lhs0 = mk_cmd_pattern value in
  let open Reserr in
  let* lhs1 =
    let ret_ty = Ir.get_return_type value in
    let* ret_ty =
      let open Ppxlib in
      match ret_ty.ptyp_desc with
      | Ptyp_var _ | Ptyp_constr _ -> ok ret_ty
      | _ ->
          error
            ( Type_not_supported (Fmt.str "%a" Pprintast.core_type ret_ty),
              ret_ty.ptyp_loc )
    in
    let* pat_ty = pat_of_core_type value.inst ret_ty in
    let pat_ret =
      match value.ret with
      | None -> ppat_any
      | Some id -> pvar (str_of_ident id)
    in
    ok
      (ppat_construct (lident "Res")
         (Some (ppat_tuple [ ppat_tuple [ pat_ty; ppat_any ]; pat_ret ])))
  in
  let lhs = ppat_tuple [ lhs0; lhs1 ] in
  let* rhs =
    let normal =
      let rec aux idx postcond =
        match (idx, postcond) with
        | [], ps -> List.map snd ps
        | i :: idx, (j, _) :: ps when i = j -> aux idx ps
        | i :: _, (j, p) :: ps ->
            assert (j < i);
            p :: aux idx ps
        | _, _ -> assert false
      in
      aux idx value.postcond.normal
    in
    let* normal =
      map
        (fun t ->
          subst_term ~gos_t:value.sut_var ~old_t:(Some state_ident) ~new_lz:true
            ~new_t:(Some new_state_ident) t
          >>= ocaml_of_term config)
        normal
    in
    ok
      (pexp_apply
         (pexp_ident (noloc (Ldot (Lident "List", "fold_left"))))
         [
           (Nolabel, pexp_ident (lident "&&"));
           (Nolabel, pexp_construct (lident "true") None);
           (Nolabel, elist normal);
         ])
  in
  (* if checks then r = Error Invalid_arg else match res with Error _ -> xpost | Ok _ -> postcond *)
  ok (case ~lhs ~guard:None ~rhs)

let postcond config idx ir =
  let cmd_name = gen_symbol ~prefix:"cmd" () in
  let state_name = gen_symbol ~prefix:"state" () in
  let res_name = gen_symbol ~prefix:"res" () in
  let new_state_name = gen_symbol ~prefix:"new_state" () in
  let new_state_let =
    pexp_let Nonrecursive
      [
        value_binding ~pat:(pvar new_state_name)
          ~expr:
            (pexp_lazy
               (pexp_apply
                  (pexp_ident (lident "next_state"))
                  [
                    (Nolabel, pexp_ident (lident cmd_name));
                    (Nolabel, pexp_ident (lident state_name));
                  ]));
      ]
  in
  let state_ident = Gospel.Tast.Ident.create ~loc:Location.none state_name in
  let new_state_ident =
    Gospel.Tast.Ident.create ~loc:Location.none new_state_name
  in
  let open Reserr in
  let* cases =
    map
      (fun v ->
        postcond_case config (List.assoc v.id idx) state_ident new_state_ident v)
      ir.values
  in
  let body =
    pexp_match (pexp_tuple [ evar cmd_name; evar res_name ]) cases
    |> new_state_let
  in
  let pat = pvar "postcond" in
  let expr =
    efun
      [
        (Nolabel, pvar cmd_name);
        (Nolabel, pvar state_name);
        (Nolabel, pvar res_name);
      ]
      body
  in
  pstr_value Nonrecursive [ value_binding ~pat ~expr ] |> ok

let cmd_constructor value =
  let name =
    String.capitalize_ascii value.id.Gospel.Tast.Ident.id_str |> noloc
  in
  let args =
    List.map (fun (ty, _) -> subst_core_type value.inst ty) value.args
  in
  constructor_declaration ~name ~args:(Pcstr_tuple args) ~res:None

let state_type ir =
  let lds =
    List.map
      (fun (id, ty) ->
        label_declaration
          ~name:(Fmt.str "%a" Gospel.Tast.Ident.pp id |> noloc)
          ~mutable_:Immutable ~type_:ty)
      ir.state
  in
  let kind = Ptype_record lds in
  let td =
    type_declaration ~name:(noloc "state") ~params:[] ~cstrs:[] ~kind
      ~private_:Public ~manifest:None
  in
  pstr_type Nonrecursive [ td ]

let cmd_type ir =
  let constructors = List.map cmd_constructor ir.values in
  let td =
    type_declaration ~name:(noloc "cmd") ~params:[] ~cstrs:[]
      ~kind:(Ptype_variant constructors) ~private_:Public ~manifest:None
  in
  pstr_type Nonrecursive [ { td with ptype_attributes = [ show_attribute ] } ]

let stm config ir =
  let cmd = cmd_type ir in
  let state = state_type ir in
  let open Reserr in
  let* idx, next_state = next_state config ir in
  let* postcond = postcond config idx ir in
  let* run = run config ir in
  let* arb_cmd = arb_cmd ir in
  ok [ cmd; state; arb_cmd; next_state; postcond; run ]
