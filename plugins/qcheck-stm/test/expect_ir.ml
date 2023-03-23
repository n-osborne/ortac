open Ortac_qcheck_stm

let () =
  let go v =
    let open Ir in
    let str =
      "Value "
      ^ v.id
      ^ " of type "
      ^ Ppxlib.string_of_core_type v.ty
      ^ " will be tested."
    in
    print_endline str
  in
  match Config.init "lib.mli" "good_init" "sut" with
  | Error err -> print_endline err
  | Ok (config, sigs) -> Ir_of_gospel.signature config sigs |> List.iter go
