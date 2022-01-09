open Runtime

let _dirac =
  let x = ref Runtime.UnInit in
  let __candelabru_fv_0 =
    fby (fun () -> Runtime.Value 1) (fun () -> Runtime.Value 0)
  in
  fun () ->
    x := __candelabru_fv_0 () ;
    !x

let%node dirac =
  let x = fby [%nostream 1] [%nostream 0] in
  x

let () =
  for _ = 0 to 9 do
    ignore (map print_int (dirac ()))
  done
