open Runtime
(*
let dirac () =
  let x = ref Runtime.UnInit in
  x := Runtime.Value (fby (fun () -> 0) (fun () -> 1)) () ;
  x *)

let%node dirac =
  let x = fby [%nostream 0] [%nostream 1] in
  x


let%node sum =
  let x = [%nostream_apply s1 + s2] in
  x
