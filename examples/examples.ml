open Runtime

let%node dirac "" =
  let x = fby [%nostream 1] [%nostream 0] in
  x

(* code génére pour dirac
   let dirac =
     let x = ref Runtime.UnInit in
     let __candelabru_fv_0 =
       fby (fun () -> Runtime.Value 1) (fun () -> Runtime.Value 0)
     in
     fun () ->
       x := __candelabru_fv_0 () ;
       !x
*)

let%node alternator "" =
  let x = fby [%nostream true] [%nostream_apply not (pre x)] in
  x

(** Code Généré pour alternator
let alternator =
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
*)

let alternate5 i = if i = 0 then 5 else 0

let%node alternator5 "" =
  let x = fby [%nostream 0] [%nostream_apply alternate5 (pre x)] in
  x

(* code généré pour alternator 5
   let alternator5 =
     let __lampadario_fv_2 = ref Runtime.UnInit in
     let __lampadario_fv_3 = ref Runtime.UnInit in
     let x = ref Runtime.UnInit in
     let __candelabru_fv_2 =
       fby (fun () -> Runtime.Value 0) (fun () -> !__lampadario_fv_2)
     in
     fun () ->
       let __pre_x = !x in
       __lampadario_fv_3 := __pre_x ;
       __lampadario_fv_2 :=
         Runtime.map
           (fun __lampadario_fv_3 -> alternate5 __lampadario_fv_3)
           !__lampadario_fv_3 ;
       x := __candelabru_fv_2 () ;
       !x
*)
let alternate4 i = if i = 0 then 4 else 0

let%node alternator4 "" =
  let x = fby [%nostream 0] [%nostream_apply alternate4 (pre x)] in
  x

(* code généré pour alternator4
   let alternator4 =
     let __lampadario_fv_4 = ref Runtime.UnInit in
     let __lampadario_fv_5 = ref Runtime.UnInit in
     let x = ref Runtime.UnInit in
     let __candelabru_fv_3 =
       fby (fun () -> Runtime.Value 0) (fun () -> !__lampadario_fv_4)
     in
     fun () ->
       let __pre_x = !x in
       __lampadario_fv_5 := __pre_x ;
       __lampadario_fv_4 :=
         Runtime.map
           (fun __lampadario_fv_5 -> alternate4 __lampadario_fv_5)
           !__lampadario_fv_5 ;
       x := __candelabru_fv_3 () ;
       !x
*)

let%node sum "s1 s2" =
  let x = [%nostream_apply s1 + s2] in
  x

(* code généré pour sum
   let sum __arg_s1 __arg_s2 =
     let x = ref Runtime.UnInit in
     let s1 = ref Runtime.UnInit in
     let s2 = ref Runtime.UnInit in
     fun () ->
       s1 := __arg_s1 () ;
       s2 := __arg_s2 () ;
       x := Runtime.bind (fun s2 -> Runtime.map (fun s1 -> s1 + s2) !s1) !s2 ;
       !x
*)

let%node summed "" =
  let x = sum alternator4 alternator5 in
  x

(* code généré pour summed
   let summed =
     let x = ref Runtime.UnInit in
     let __candelabru_fv_4 = sum alternator4 alternator5 in
     fun () ->
       x := __candelabru_fv_4 () ;
       !x
*)

let%node if_test1 "" =
  let x = [%nostream value ~default:true (alternator ())]
  and y = if x then [%nostream 4] else [%nostream 5] in
  y

(* code généré pour if_test1
   let if_test1 =
   let x = ref Runtime.UnInit in
   let y = ref Runtime.UnInit in
   fun () ->
     x := Runtime.Value (value ~default:true (alternator ())) ;
     y :=
       Runtime.bind (fun x -> if x then Runtime.Value 4 else Runtime.Value 5) !x ;
     !y
*)
let%node if_test2 "" =
  let x = fby [%nostream true] [%nostream_apply not (pre x)]
  and y = if x then [%nostream 4] else [%nostream 5] in
  y

(* code généré pour if_test2
   let if_test2 =
     let __lampadario_fv_6 = ref Runtime.UnInit in
     let __lampadario_fv_7 = ref Runtime.UnInit in
     let x = ref Runtime.UnInit in
     let y = ref Runtime.UnInit in
     let __candelabru_fv_5 =
       fby (fun () -> Runtime.Value true) (fun () -> !__lampadario_fv_6)
     in
     fun () ->
       let __pre_x = !x in
       __lampadario_fv_7 := __pre_x ;
       __lampadario_fv_6 :=
         Runtime.map
           (fun __lampadario_fv_7 -> not __lampadario_fv_7)
           !__lampadario_fv_7 ;
       x := __candelabru_fv_5 () ;
       y :=
         Runtime.bind (fun x -> if x then Runtime.Value 4 else Runtime.Value 5) !x ;
       !y
*)

let print_bool b = print_string (if b then "true" else "false")

let () =
  print_string
    "Test de dirac \n resultat attendu : 1000000000\n resultat obtenu : " ;
  for _ = 0 to 9 do
    ignore (map print_int (dirac ()))
  done ;
  print_newline () ;
  print_string
    "Test de alternateur \n\
    \ resultat attendu : true false true false true ...\n\
    \ resultat obtenu : " ;
  for _ = 0 to 9 do
    ignore (map print_bool (alternator ())) ;
    print_string " "
  done ;
  print_newline () ;
  print_string
    "Test de alternateur5 et alternateur4 \n\
    \ resultat attendu : 05050505... et 0404040404...\n\
    \ resultat obtenu : " ;
  for _ = 0 to 9 do
    ignore (map print_int (alternator5 ()))
  done ;
  print_string " et " ;
  for _ = 0 to 9 do
    ignore (map print_int (alternator4 ()))
  done ;
  print_newline () ;
  print_string
    "Test de summed \n resultat attendu : 09090909 ...\n resultat obtenu : " ;
  for _ = 0 to 9 do
    ignore (map print_int (summed ()))
  done ;
  print_newline () ;
  print_string
    "Test de if_test1 \n resultat attendu : 4545454545 ...\n resultat obtenu : " ;
  for _ = 0 to 9 do
    ignore (map print_int (if_test1 ()))
  done ;
  print_newline () ;
  print_string
    "Test de if_test2 \n resultat attendu : 4545454545 ...\n resultat obtenu : " ;
  for _ = 0 to 9 do
    ignore (map print_int (if_test2 ()))
  done ;
  print_newline ()
