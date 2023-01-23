include Stm_example
open QCheck
open STM
module Gospel_stdlib =
  struct
    let max_int = Z.of_int Stdlib.Int.max_int
    let min_int = Z.of_int Stdlib.Int.min_int
    module List =
      struct
        let length l = (Stdlib.List.length l) |> Z.of_int
        let mapi f l =
          Stdlib.List.mapi (fun i -> fun a -> f (Z.of_int i) a) l
        let init i f =
          Stdlib.List.init (Z.to_int i) (fun i -> f (Z.of_int i))
        let nth l i = Stdlib.List.nth l (Z.to_int i)
      end
  end
module Conf =
  struct
    type sut = Stm_example.t
    type state = {
      state: Z.t }
    type cmd =
      | Add_sut of int [@@deriving show { with_path = false }]
    let init_sut = Stm_example.init_sut
    let cleanup _ = ()
    let arb_cmd _s =
      QCheck.make ~print:show_cmd
        (let open Gen in
           oneof
             [Gen.map (fun i -> Add_sut i)
                (frequency [(1, small_nat); (20, int)])])
    let next_state c s =
      match c with
      | Add_sut i ->
          let old_state = s in
          { state = (Z.add old_state.state (Z.of_int i)) }
    let run c sut =
      match c with
      | Add_sut i ->
          Res ((result int exn), (protect (Stm_example.add_sut sut) i))
    let init_state = { state = (Z.of_int 0) }
    let precond c s = match c with | Add_sut i -> let t_2 = s in true
    let postcond c s res_1 =
      match (c, res_1) with
      | (Add_sut i, Res ((Result (Int, Exn), _), res)) ->
          let old_state = s in
          let t_2 = next_state c s in
          if true
          then
            (match res with
             | Error exn ->
                 (match exn with
                  | Stack_overflow | Out_of_memory -> raise exn
                  | _ -> false)
             | Ok res ->
                 ((Z.sub t_2.state (Z.of_int res)) = (Z.of_int 0)) &&
                   ((let __t1__001_ = Z.geq t_2.state (Z.of_int 0) in
                     let __t2__002_ =
                       t_2.state = (Z.add old_state.state (Z.of_int i)) in
                     __t1__001_ && __t2__002_) &&
                      (((Z.sub t_2.state old_state.state) = (Z.of_int i)) &&
                         (((Z.sub t_2.state (Z.of_int i)) = old_state.state)
                            && (t_2.state = (Z.of_int res))))))
          else
            (match res with | Error (Invalid_argument _) -> true | _ -> false)
      | _ -> false
  end
module Test = (STM.Make)(Conf)
let _ =
  QCheck_runner.run_tests_main
    (let (count, name) = (1000, "Stm_example test") in
     [Test.agree_test ~count ~name; Test.agree_test_par ~count ~name])
