open Ortac_core

let signature ~runtime ~module_name namespace (s : Gospel.Tast.signature) =
  let driver = Drv.init_stm module_name namespace in
  let (translated: Drv.t) = Phase1.signature ~driver s in
  Report.emit_warnings Fmt.stderr translated;
  let stm =  Phase2.stm translated in
  Phase3.structure runtime stm


let generate path output = 
let module_name = Ortac_core.Utils.module_name_of_path path in
  let output = Format.formatter_of_out_channel output in
  Gospel.Parser_frontend.parse_ocaml_gospel path
  |> Ortac_core.Utils.type_check [] path
  |> fun (env, sigs) ->
  assert (List.length env = 1);
signature ~runtime:"Gospel_stdlib" ~module_name (List.hd env)
      sigs
     |> Fmt.pf output "%a@." Ppxlib_ast.Pprintast.structure
