#!/bin/bash

dune exec --display=quiet examples/varray_tests2.exe | head -n -3 | tail -n +7 | awk 'BEGIN { print "let () ="; print "  let open Varray in"; print "  let a = make 42 (Char.chr 97) in"; print "  try" } { cmd = $1; $1 = ""; print "    let _ = "cmd" a "$0" in" } END { print "    ()\n  with _ -> ()" }' > examples/reproduce.ml
