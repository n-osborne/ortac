let () =
  let open Varray in
  let a = make 42 (Char.chr 97) in
  try
    let _ = push_front a  '\247' in
    let _ = delete_at a  85 in
    let _ = push_back a  'r' in
    ()
  with _ -> ()
