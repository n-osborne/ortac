open Lwt_dllist_spec

module Spec = struct
  open STM

  (* Hand-added portion *)
  type _ ty += Node : 'a ty -> 'a node ty

  let node spec =
    let ty, show = spec in
    (Node ty, fun n -> Printf.sprintf "Node %s" (show (get n)))

  (* End of hand-added portion *)
  [@@@ocaml.warning "-26-27"]

  type sut = int t

  type cmd =
    | Is_empty
    | Length
    | Add_l of int
    | Add_r of int
    | Take_l
    | Take_r
    | Take_opt_l
    | Take_opt_r

  let show_cmd cmd__001_ =
    match cmd__001_ with
    | Is_empty -> Format.asprintf "%s" "is_empty"
    | Length -> Format.asprintf "%s" "length"
    | Add_l a_1 -> Format.asprintf "%s %a" "add_l" (Util.Pp.pp_int true) a_1
    | Add_r a_2 -> Format.asprintf "%s %a" "add_r" (Util.Pp.pp_int true) a_2
    | Take_l -> Format.asprintf "%s" "take_l"
    | Take_r -> Format.asprintf "%s" "take_r"
    | Take_opt_l -> Format.asprintf "%s" "take_opt_l"
    | Take_opt_r -> Format.asprintf "%s" "take_opt_r"

  type nonrec state = { contents : int Ortac_runtime.Gospelstdlib.sequence }

  let init_state =
    let () = () in
    {
      contents =
        (try Ortac_runtime.Gospelstdlib.Sequence.empty
         with e ->
           raise
             (Ortac_runtime.Partial_function
                ( e,
                  {
                    Ortac_runtime.start =
                      {
                        pos_fname = "lwt_dllist_spec.mli";
                        pos_lnum = 40;
                        pos_bol = 1680;
                        pos_cnum = 1707;
                      };
                    Ortac_runtime.stop =
                      {
                        pos_fname = "lwt_dllist_spec.mli";
                        pos_lnum = 40;
                        pos_bol = 1680;
                        pos_cnum = 1721;
                      };
                  } )));
    }

  let init_sut () = create ()
  let cleanup _ = ()

  let arb_cmd _ =
    let open QCheck in
    make ~print:show_cmd
      (let open Gen in
       oneof
         [
           pure Is_empty;
           pure Length;
           pure (fun a_1 -> Add_l a_1) <*> int;
           pure (fun a_2 -> Add_r a_2) <*> int;
           pure Take_l;
           pure Take_r;
           pure Take_opt_l;
           pure Take_opt_r;
         ])

  let next_state cmd__002_ state__003_ =
    match cmd__002_ with
    | Is_empty -> state__003_
    | Length -> state__003_
    | Add_l a_1 ->
        {
          contents =
            (try
               Ortac_runtime.Gospelstdlib.Sequence.cons a_1 state__003_.contents
             with e ->
               raise
                 (Ortac_runtime.Partial_function
                    ( e,
                      {
                        Ortac_runtime.start =
                          {
                            pos_fname = "lwt_dllist_spec.mli";
                            pos_lnum = 58;
                            pos_bol = 2958;
                            pos_cnum = 2985;
                          };
                        Ortac_runtime.stop =
                          {
                            pos_fname = "lwt_dllist_spec.mli";
                            pos_lnum = 58;
                            pos_bol = 2958;
                            pos_cnum = 3017;
                          };
                      } )));
        }
    | Add_r a_2 ->
        {
          contents =
            (try
               Ortac_runtime.Gospelstdlib.Sequence.snoc state__003_.contents a_2
             with e ->
               raise
                 (Ortac_runtime.Partial_function
                    ( e,
                      {
                        Ortac_runtime.start =
                          {
                            pos_fname = "lwt_dllist_spec.mli";
                            pos_lnum = 64;
                            pos_bol = 3426;
                            pos_cnum = 3453;
                          };
                        Ortac_runtime.stop =
                          {
                            pos_fname = "lwt_dllist_spec.mli";
                            pos_lnum = 64;
                            pos_bol = 3426;
                            pos_cnum = 3485;
                          };
                      } )));
        }
    | Take_l ->
        {
          contents =
            (try
               if
                 state__003_.contents
                 = Ortac_runtime.Gospelstdlib.Sequence.empty
               then Ortac_runtime.Gospelstdlib.Sequence.empty
               else Ortac_runtime.Gospelstdlib.Sequence.tl state__003_.contents
             with e ->
               raise
                 (Ortac_runtime.Partial_function
                    ( e,
                      {
                        Ortac_runtime.start =
                          {
                            pos_fname = "lwt_dllist_spec.mli";
                            pos_lnum = 76;
                            pos_bol = 4023;
                            pos_cnum = 4050;
                          };
                        Ortac_runtime.stop =
                          {
                            pos_fname = "lwt_dllist_spec.mli";
                            pos_lnum = 78;
                            pos_bol = 4132;
                            pos_cnum = 4192;
                          };
                      } )));
        }
    | Take_r ->
        {
          contents =
            (try
               if
                 state__003_.contents
                 = Ortac_runtime.Gospelstdlib.Sequence.empty
               then Ortac_runtime.Gospelstdlib.Sequence.empty
               else
                 Ortac_runtime.Gospelstdlib.__mix_Bddub state__003_.contents
                   (Ortac_runtime.Gospelstdlib.( - )
                      (Ortac_runtime.Gospelstdlib.Sequence.length
                         state__003_.contents)
                      (Ortac_runtime.Gospelstdlib.integer_of_int 2))
             with e ->
               raise
                 (Ortac_runtime.Partial_function
                    ( e,
                      {
                        Ortac_runtime.start =
                          {
                            pos_fname = "lwt_dllist_spec.mli";
                            pos_lnum = 90;
                            pos_bol = 4826;
                            pos_cnum = 4853;
                          };
                        Ortac_runtime.stop =
                          {
                            pos_fname = "lwt_dllist_spec.mli";
                            pos_lnum = 92;
                            pos_bol = 4935;
                            pos_cnum = 5025;
                          };
                      } )));
        }
    | Take_opt_l ->
        {
          contents =
            (try
               if
                 Ortac_runtime.Gospelstdlib.Sequence.length state__003_.contents
                 = Ortac_runtime.Gospelstdlib.integer_of_int 0
               then Ortac_runtime.Gospelstdlib.Sequence.empty
               else Ortac_runtime.Gospelstdlib.Sequence.tl state__003_.contents
             with e ->
               raise
                 (Ortac_runtime.Partial_function
                    ( e,
                      {
                        Ortac_runtime.start =
                          {
                            pos_fname = "lwt_dllist_spec.mli";
                            pos_lnum = 103;
                            pos_bol = 5726;
                            pos_cnum = 5753;
                          };
                        Ortac_runtime.stop =
                          {
                            pos_fname = "lwt_dllist_spec.mli";
                            pos_lnum = 105;
                            pos_bol = 5840;
                            pos_cnum = 5900;
                          };
                      } )));
        }
    | Take_opt_r ->
        {
          contents =
            (try
               if
                 Ortac_runtime.Gospelstdlib.Sequence.length state__003_.contents
                 = Ortac_runtime.Gospelstdlib.integer_of_int 0
               then Ortac_runtime.Gospelstdlib.Sequence.empty
               else
                 Ortac_runtime.Gospelstdlib.__mix_Bddub state__003_.contents
                   (Ortac_runtime.Gospelstdlib.( - )
                      (Ortac_runtime.Gospelstdlib.Sequence.length
                         state__003_.contents)
                      (Ortac_runtime.Gospelstdlib.integer_of_int 2))
             with e ->
               raise
                 (Ortac_runtime.Partial_function
                    ( e,
                      {
                        Ortac_runtime.start =
                          {
                            pos_fname = "lwt_dllist_spec.mli";
                            pos_lnum = 115;
                            pos_bol = 6562;
                            pos_cnum = 6589;
                          };
                        Ortac_runtime.stop =
                          {
                            pos_fname = "lwt_dllist_spec.mli";
                            pos_lnum = 117;
                            pos_bol = 6676;
                            pos_cnum = 6766;
                          };
                      } )));
        }

  let precond cmd__012_ state__013_ =
    match cmd__012_ with
    | Is_empty -> true
    | Length -> true
    | Add_l a_1 -> true
    | Add_r a_2 -> true
    | Take_l -> true
    | Take_r -> true
    | Take_opt_l -> true
    | Take_opt_r -> true

  let postcond cmd__004_ state__005_ res__006_ =
    let new_state__007_ = lazy (next_state cmd__004_ state__005_) in
    match (cmd__004_, res__006_) with
    | Is_empty, Res ((Bool, _), b) -> (
        try
          b
          = true
          = ((Lazy.force new_state__007_).contents
            = Ortac_runtime.Gospelstdlib.Sequence.empty)
        with e ->
          raise
            (Ortac_runtime.Partial_function
               ( e,
                 {
                   Ortac_runtime.start =
                     {
                       pos_fname = "lwt_dllist_spec.mli";
                       pos_lnum = 45;
                       pos_bol = 2067;
                       pos_cnum = 2081;
                     };
                   Ortac_runtime.stop =
                     {
                       pos_fname = "lwt_dllist_spec.mli";
                       pos_lnum = 45;
                       pos_bol = 2067;
                       pos_cnum = 2114;
                     };
                 } )))
    | Length, Res ((Int, _), l) -> (
        try
          Ortac_runtime.Gospelstdlib.integer_of_int l
          = Ortac_runtime.Gospelstdlib.Sequence.length
              (Lazy.force new_state__007_).contents
        with e ->
          raise
            (Ortac_runtime.Partial_function
               ( e,
                 {
                   Ortac_runtime.start =
                     {
                       pos_fname = "lwt_dllist_spec.mli";
                       pos_lnum = 52;
                       pos_bol = 2522;
                       pos_cnum = 2536;
                     };
                   Ortac_runtime.stop =
                     {
                       pos_fname = "lwt_dllist_spec.mli";
                       pos_lnum = 52;
                       pos_bol = 2522;
                       pos_cnum = 2566;
                     };
                 } )))
    | Add_l a_1, Res ((Node Int, _), n) -> true
    | Add_r a_2, Res ((Node Int, _), n_1) -> true
    | Take_l, Res ((Result (Int, Exn), _), a_3) -> (
        match a_3 with
        | Ok a_3 -> (
            try
              if
                state__005_.contents = Ortac_runtime.Gospelstdlib.Sequence.empty
              then false
              else
                a_3
                = Ortac_runtime.Gospelstdlib.Sequence.hd state__005_.contents
            with e ->
              raise
                (Ortac_runtime.Partial_function
                   ( e,
                     {
                       Ortac_runtime.start =
                         {
                           pos_fname = "lwt_dllist_spec.mli";
                           pos_lnum = 79;
                           pos_bol = 4193;
                           pos_cnum = 4207;
                         };
                       Ortac_runtime.stop =
                         {
                           pos_fname = "lwt_dllist_spec.mli";
                           pos_lnum = 81;
                           pos_bol = 4267;
                           pos_cnum = 4318;
                         };
                     } )))
        | Error Empty -> (
            try
              let __t1__008_ =
                state__005_.contents = Ortac_runtime.Gospelstdlib.Sequence.empty
              in
              let __t2__009_ =
                Ortac_runtime.Gospelstdlib.Sequence.empty
                = (Lazy.force new_state__007_).contents
              in
              __t1__008_ && __t2__009_
            with e ->
              raise
                (Ortac_runtime.Partial_function
                   ( e,
                     {
                       Ortac_runtime.start =
                         {
                           pos_fname = "lwt_dllist_spec.mli";
                           pos_lnum = 82;
                           pos_bol = 4319;
                           pos_cnum = 4341;
                         };
                       Ortac_runtime.stop =
                         {
                           pos_fname = "lwt_dllist_spec.mli";
                           pos_lnum = 82;
                           pos_bol = 4319;
                           pos_cnum = 4385;
                         };
                     } )))
        | _ -> false)
    | Take_r, Res ((Result (Int, Exn), _), a_4) -> (
        match a_4 with
        | Ok a_4 -> (
            try
              if
                state__005_.contents = Ortac_runtime.Gospelstdlib.Sequence.empty
              then false
              else
                a_4
                = Ortac_runtime.Gospelstdlib.__mix_Bub state__005_.contents
                    (Ortac_runtime.Gospelstdlib.( - )
                       (Ortac_runtime.Gospelstdlib.Sequence.length
                          state__005_.contents)
                       (Ortac_runtime.Gospelstdlib.integer_of_int 1))
            with e ->
              raise
                (Ortac_runtime.Partial_function
                   ( e,
                     {
                       Ortac_runtime.start =
                         {
                           pos_fname = "lwt_dllist_spec.mli";
                           pos_lnum = 93;
                           pos_bol = 5026;
                           pos_cnum = 5040;
                         };
                       Ortac_runtime.stop =
                         {
                           pos_fname = "lwt_dllist_spec.mli";
                           pos_lnum = 95;
                           pos_bol = 5100;
                           pos_cnum = 5177;
                         };
                     } )))
        | Error Empty -> (
            try
              let __t1__010_ =
                state__005_.contents = Ortac_runtime.Gospelstdlib.Sequence.empty
              in
              let __t2__011_ =
                Ortac_runtime.Gospelstdlib.Sequence.empty
                = (Lazy.force new_state__007_).contents
              in
              __t1__010_ && __t2__011_
            with e ->
              raise
                (Ortac_runtime.Partial_function
                   ( e,
                     {
                       Ortac_runtime.start =
                         {
                           pos_fname = "lwt_dllist_spec.mli";
                           pos_lnum = 96;
                           pos_bol = 5178;
                           pos_cnum = 5200;
                         };
                       Ortac_runtime.stop =
                         {
                           pos_fname = "lwt_dllist_spec.mli";
                           pos_lnum = 96;
                           pos_bol = 5178;
                           pos_cnum = 5244;
                         };
                     } )))
        | _ -> false)
    | Take_opt_l, Res ((Option Int, _), o) -> (
        try
          state__005_.contents
          =
          match o with
          | None -> Ortac_runtime.Gospelstdlib.Sequence.empty
          | Some a_5 ->
              Ortac_runtime.Gospelstdlib.Sequence.cons a_5
                (Lazy.force new_state__007_).contents
        with e ->
          raise
            (Ortac_runtime.Partial_function
               ( e,
                 {
                   Ortac_runtime.start =
                     {
                       pos_fname = "lwt_dllist_spec.mli";
                       pos_lnum = 106;
                       pos_bol = 5901;
                       pos_cnum = 5915;
                     };
                   Ortac_runtime.stop =
                     {
                       pos_fname = "lwt_dllist_spec.mli";
                       pos_lnum = 108;
                       pos_bol = 6002;
                       pos_cnum = 6072;
                     };
                 } )))
    | Take_opt_r, Res ((Option Int, _), o_1) -> (
        try
          state__005_.contents
          =
          match o_1 with
          | None -> Ortac_runtime.Gospelstdlib.Sequence.empty
          | Some a_6 ->
              Ortac_runtime.Gospelstdlib.Sequence.snoc
                (Lazy.force new_state__007_).contents a_6
        with e ->
          raise
            (Ortac_runtime.Partial_function
               ( e,
                 {
                   Ortac_runtime.start =
                     {
                       pos_fname = "lwt_dllist_spec.mli";
                       pos_lnum = 118;
                       pos_bol = 6767;
                       pos_cnum = 6781;
                     };
                   Ortac_runtime.stop =
                     {
                       pos_fname = "lwt_dllist_spec.mli";
                       pos_lnum = 120;
                       pos_bol = 6868;
                       pos_cnum = 6938;
                     };
                 } )))
    | _ -> true

  let run cmd__014_ sut__015_ =
    match cmd__014_ with
    | Is_empty -> Res (bool, is_empty sut__015_)
    | Length -> Res (int, length sut__015_)
    | Add_l a_1 -> Res (node int, add_l a_1 sut__015_)
    | Add_r a_2 -> Res (node int, add_r a_2 sut__015_)
    | Take_l -> Res (result int exn, protect (fun () -> take_l sut__015_) ())
    | Take_r -> Res (result int exn, protect (fun () -> take_r sut__015_) ())
    | Take_opt_l -> Res (option int, take_opt_l sut__015_)
    | Take_opt_r -> Res (option int, take_opt_r sut__015_)
end

module STMTests = STM_sequential.Make (Spec)

let _ =
  QCheck_base_runner.run_tests_main
    (let count = 1000 in
     [ STMTests.agree_test ~count ~name:"Lwt_dllist_spec STM tests" ])
