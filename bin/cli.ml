type fontend = Default | Monolith

let frontend_printer f = function
  | Default -> Format.pp_print_string f "Default"
  | Monolith -> Format.pp_print_string f "Monolith"

let frontend_parser = function
  | "default" -> Ok Default
  | "monolith" -> Ok Monolith
  | s -> Error (`Msg (Printf.sprintf "Error: `%s' is not a valid argument" s))

let main = function
  | Default -> Ortac_default.generate
  | Monolith -> Ortac_monolith.generate

open Cmdliner

let ocaml_file =
  let parse s =
    match Sys.file_exists s with
    | true ->
        if Sys.is_directory s (* || Filename.extension s <> ".mli" *) then
          `Error (Printf.sprintf "Error: `%s' is not an OCaml interface file" s)
        else `Ok s
    | false -> `Error (Printf.sprintf "Error: `%s' not found" s)
  in
  Arg.(
    required
    & pos 0 (some (parse, Format.pp_print_string)) None
    & info [] ~docv:"FILE")

let frontend =
  Arg.(
    value
    & opt (conv ~docv:"FRONTEND" (frontend_parser, frontend_printer)) Default
    & info [ "f"; "frontend" ] ~docv:"FRONTEND")

let cmd =
  let doc = "Run ORTAC." in
  (Term.(const main $ frontend $ ocaml_file), Term.info "ortac" ~doc)

let () = Term.(exit @@ eval cmd)
