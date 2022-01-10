open Runtime

let _dirac =
  let x = ref Runtime.UnInit in
  let __candelabru_fv_0 =
    fby (fun () -> Runtime.Value 1) (fun () -> Runtime.Value 0)
  in
  fun () ->
    x := __candelabru_fv_0 () ;
    !x

let _dirac =
  let x = ref Runtime.UnInit in
  let __candelabru_fv_0 =
    fby (fun () -> Runtime.Value 1) (fun () -> Runtime.Value 0)
  in
  fun () ->
    x := __candelabru_fv_0 () ;
    !x

let%node dirac "" =
  let x = fby [%nostream 1] [%nostream 0] in
  x

(*let _alternator =
  let __lampadario_fv_0 = ref Runtime.UnInit in
  let __lampadario_fv_1 = ref Runtime.UnInit in
  let x = ref Runtime.UnInit in
  let __kroonluchter_fv_2 = !__lampadario_fv_0 in
  let __kroonluchter_fv_1 = !__lampadario_fv_1 in
  let __candelabru_fv_1 =
    fby (fun () -> Runtime.Value true) __kroonluchter_fv_2
  in
  fun () ->
    let __kroonluchter_fv_2 = !__lampadario_fv_0 in
    let __kroonluchter_fv_1 = !__lampadario_fv_1 in
    __lampadario_fv_1 := Runtime.Value __kroonluchter_fv_0 ;
    let __kroonluchter_fv_2 = !__lampadario_fv_0 in
    let __kroonluchter_fv_1 = !__lampadario_fv_1 in
    __lampadario_fv_0 :=
      Runtime.map
        (fun __candelabru_fv_0 -> not __kroonluchter_fv_1 ())
        __kroonluchter_fv_1 ;
    let __kroonluchter_fv_2 = !__lampadario_fv_0 in
    let __kroonluchter_fv_1 = !__lampadario_fv_1 in
    x := __candelabru_fv_1 () ;
    x := __kroonluchter_fv_0 ;
    !x*)

let _testpre =
  let x = ref Runtime.UnInit in
  let y = ref Runtime.UnInit in
  fun () ->
    let __kroonluchter_fv_0 = !y in
    y := Runtime.Value 1 ;
    x := __kroonluchter_fv_0 ;
    !x
(*
let%node testpre =
  let x = pre y and y = [%nostream 1] in
  x *)

let _alt =
  let __lampadario_fv_0 = ref Runtime.UnInit in
  let __lampadario_fv_1 = ref Runtime.UnInit in
  let x = ref Runtime.UnInit in
  let __candelabru_fv_1 =
    fby (fun () -> Runtime.Value true) (fun () -> !__lampadario_fv_0)
  in
  fun () ->
    let __pre_x = !x in
    __lampadario_fv_1 := __pre_x ;
    __lampadario_fv_0 :=
      Runtime.map
        (fun __lampadario_fv_1 -> not __lampadario_fv_1)
        !__lampadario_fv_1 ;
    x := __candelabru_fv_1 () ;
    !x

let%node _alternator "" =
  let x = fby [%nostream true] [%nostream_apply not (pre x)] in
  x

let alternate5 i = if i = 0 then 5 else 0

let%node alternator5 "" =
  let x = fby [%nostream 0] [%nostream_apply alternate5 (pre x)] in
  x

let alternate4 i = if i = 0 then 4 else 0

let%node alternator4 "" =
  let x = fby [%nostream 0] [%nostream_apply alternate4 (pre x)] in
  x

let%node sum "s1 s2" =
  let x = [%nostream_apply s1 + s2] in
  x

let%node summed "" =
  let x = sum alternator4 alternator5 in
  x

let print_bool b = print_endline (if b then "true" else "false")

let () =
  for _ = 0 to 9 do
    ignore (map print_int (summed ()))
  done

let () = print_newline ()

let () =
  for _ = 0 to 9 do
    ignore (map print_int (dirac ()))
  done