let fresh_ident prefix =
  let i = ref (-1) in
  fun () ->
    i := !i + 1 ;
    Printf.sprintf "%s%i" prefix !i

module String = String

include Printf