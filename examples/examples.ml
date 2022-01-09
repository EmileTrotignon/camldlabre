open Runtime

let f f' a = f' a

let g = ( @@ )

let dirac () =
  let __lampadario_fv_0 = ref Runtime.UnInit in
  let __lampadario_fv_1 = ref Runtime.UnInit in
  let __lampadario_fv_2 = ref Runtime.UnInit in
  let x = ref Runtime.UnInit in
  let __kroonluchter_fv_2 = !__lampadario_fv_2 in
  let __kroonluchter_fv_1 = !__lampadario_fv_1 in
  let __kroonluchter_fv_0 = !__lampadario_fv_0 in
  __lampadario_fv_0 := Runtime.Value fby ;
  let __kroonluchter_fv_2 = !__lampadario_fv_2 in
  let __kroonluchter_fv_1 = !__lampadario_fv_1 in
  let __kroonluchter_fv_0 = !__lampadario_fv_0 in
  __lampadario_fv_1 := Runtime.Value (fun () -> 1) ;
  let __kroonluchter_fv_2 = !__lampadario_fv_2 in
  let __kroonluchter_fv_1 = !__lampadario_fv_1 in
  let __kroonluchter_fv_0 = !__lampadario_fv_0 in
  __lampadario_fv_2 := Runtime.Value (fun () -> 0) ;
  let __kroonluchter_fv_2 = !__lampadario_fv_2 in
  let __kroonluchter_fv_1 = !__lampadario_fv_1 in
  let __kroonluchter_fv_0 = !__lampadario_fv_0 in
  x :=
    Runtime.bind
      (fun __kroonluchter_fv_0 ->
        Runtime.bind
          (fun __kroonluchter_fv_1 ->
            Runtime.map
              (fun __kroonluchter_fv_2 ->
                __kroonluchter_fv_0 __kroonluchter_fv_1 __kroonluchter_fv_2 ()
                )
              __kroonluchter_fv_2 )
          __kroonluchter_fv_1 )
      __kroonluchter_fv_0 ;
  !x
(*
let _dirac () () =
  let __lampadario_fv_0 = ref Runtime.UnInit in
  let __lampadario_fv_1 = ref Runtime.UnInit in
  let __lampadario_fv_2 = ref Runtime.UnInit in
  let x = ref Runtime.UnInit in
  let __kroonluchter_fv_2 = !__lampadario_fv_2 in
  let __kroonluchter_fv_1 = !__lampadario_fv_1 in
  let __kroonluchter_fv_0 = !__lampadario_fv_0 in
  __lampadario_fv_0 := Runtime.Value fby ;
  let __kroonluchter_fv_2 = !__lampadario_fv_2 in
  let __kroonluchter_fv_1 = !__lampadario_fv_1 in
  let __kroonluchter_fv_0 = !__lampadario_fv_0 in
  __lampadario_fv_1 := Runtime.Value (fun () -> 0) ;
  let __kroonluchter_fv_2 = !__lampadario_fv_2 in
  let __kroonluchter_fv_1 = !__lampadario_fv_1 in
  let __kroonluchter_fv_0 = !__lampadario_fv_0 in
  __lampadario_fv_2 := Runtime.Value (fun () -> 1) ;
  let __kroonluchter_fv_2 = !__lampadario_fv_2 in
  let __kroonluchter_fv_1 = !__lampadario_fv_1 in
  let __kroonluchter_fv_0 = !__lampadario_fv_0 in
  x :=
    Runtime.bind
      (fun __kroonluchter_fv_0 ->
        Runtime.bind
          (fun __kroonluchter_fv_1 ->
            Runtime.map
              (fun __kroonluchter_fv_2 ->
                __kroonluchter_fv_0 __kroonluchter_fv_1 __kroonluchter_fv_2 ()
                )
              __kroonluchter_fv_2 )
          __kroonluchter_fv_1 )
      __kroonluchter_fv_0 ;
  !x

let _dirac () () =
  let __lampadario_fv_0 = ref Runtime.UnInit in
  let __lampadario_fv_1 = ref Runtime.UnInit in
  let __lampadario_fv_2 = ref Runtime.UnInit in
  let x = ref Runtime.UnInit in
  let __kroonluchter_fv_2 = !__lampadario_fv_2 in
  let __kroonluchter_fv_1 = !__lampadario_fv_1 in
  let __kroonluchter_fv_0 = !__lampadario_fv_0 in
  __lampadario_fv_0 := Runtime.Value fby ;
  let __kroonluchter_fv_2 = !__lampadario_fv_2 in
  let __kroonluchter_fv_1 = !__lampadario_fv_1 in
  let __kroonluchter_fv_0 = !__lampadario_fv_0 in
  __lampadario_fv_1 := Runtime.Value (fun () -> 0) ;
  let __kroonluchter_fv_2 = !__lampadario_fv_2 in
  let __kroonluchter_fv_1 = !__lampadario_fv_1 in
  let __kroonluchter_fv_0 = !__lampadario_fv_0 in
  __lampadario_fv_2 := Runtime.Value (fun () -> 1) ;
  let fv_trucmuch

  let __kroonluchter_fv_2 = !__lampadario_fv_2 in
  let __kroonluchter_fv_1 = !__lampadario_fv_1 in
  let __kroonluchter_fv_0 = !__lampadario_fv_0 in
  x :=
    Runtime.bind
      (fun __kroonluchter_fv_0 ->
        Runtime.bind
          (fun __kroonluchter_fv_1 ->
            Runtime.map
              (fun __kroonluchter_fv_2 ->
                __kroonluchter_fv_0 __kroonluchter_fv_1 __kroonluchter_fv_2 ()
                )
              __kroonluchter_fv_2 )
          __kroonluchter_fv_1 )
      __kroonluchter_fv_0 ;
  !x
*)
(*
let dirac () =
  let __lampadario_fv_0 = ref Runtime.UnInit in
  let __lampadario_fv_1 = ref Runtime.UnInit in
  let __lampadario_fv_2 = ref Runtime.UnInit in
  let x = ref Runtime.UnInit in
  __lampadario_fv_0 := Runtime.Value fby ;
  __lampadario_fv_1 := Runtime.Value (fun () -> 0) ;
  __lampadario_fv_2 := Runtime.Value (fun () -> 1) ;
  x := Runtime.Value (__lampadario_fv_0 __lampadario_fv_1 __lampadario_fv_2) ;
  x *)
(*
let dirac () =
  let x = ref Runtime.UnInit in
  x := Runtime.Value (fby (fun () -> 0) (fun () -> 1)) () ;
  x *)

let%node dirac =
  let x = fby [%nostream 1] [%nostream 0] in
  x

let () =
  for _ = 0 to 9 do
    ignore (map print_int (dirac ()))
  done
