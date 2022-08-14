val type_of_ty : driver:Drv.t -> Gospel.Ttypes.ty -> Translated.type_

val var_of_arg: driver:Drv.t -> Gospel.Tast.lb_arg -> Translated.ocaml_var

val signature : driver:Drv.t -> Gospel.Tast.signature -> Drv.t
