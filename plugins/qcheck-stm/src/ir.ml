open Gospel
module Ident = Identifier.Ident

type xpost = Ttypes.xsymbol * (Tterm.pattern * Tterm.term) list

type next_state = {
  formulae : Tterm.term list;
  modifies : Tterm.term list;
  checks : Tterm.term list;
}

type postcond = {
  normal : Tterm.term list;
  exceptional : xpost list;
  checks : Tterm.term list;
}

type value = {
  id : Ident.t;
  ty : Ppxlib.core_type;
  inst : (string * Ppxlib.core_type) list;
  sut_var : Ident.t;
  args : Ident.t list;
  next_state : next_state;
  postcond : postcond;
  precond : Tterm.term list;
}

let value id ty inst =
  {
    id;
    ty;
    inst;
    sut_var = Ident.create ~loc:Location.none "dummy_sut_var";
    args = [];
    next_state = { formulae = []; modifies = []; checks = [] };
    postcond = { normal = []; exceptional = []; checks = [] };
    precond = [];
  }

let pp_value ppf v = Fmt.(pf ppf "id = %a@." Ident.pp v.id)
