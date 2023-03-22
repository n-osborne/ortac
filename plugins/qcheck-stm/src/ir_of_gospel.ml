open Gospel
open Tast
open Config
open Ir

let val_desc config vd =
  let open Identifier.Ident in
  if vd.vd_name = config.init then None else Some { id = vd.vd_name.id_str }

let sig_item config s =
  match s.sig_desc with
  | Sig_val (vd, Nonghost) -> val_desc config vd
  | _ -> None

let signature config = List.filter_map (sig_item config)
