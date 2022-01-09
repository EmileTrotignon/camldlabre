(* This is just a fonction to create fresh name, in order to avoid conflict of notation*)
let fresh_ident prefix =
  let i = ref (-1) in
  fun () ->
    i := !i + 1 ;
    Printf.sprintf "%s%i" prefix !i

module String = String
