open Ortac_qcheck_stm

let () =
  match Config.init "lib.mli" "good_init" "sut" with
  | Error err -> print_endline err
  | Ok (config, sigs) ->
      let ir = Ir_of_gospel.signature config sigs in
      List.iter
        (fun v -> print_endline ("Value " ^ v.Ir.id ^ " will be tested"))
        ir
