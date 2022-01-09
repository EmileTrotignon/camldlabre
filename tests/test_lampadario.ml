(* open Lampadario

let test_order_eqs () =
  Alcotest.(check (list (pair string string))
    "equations" Compile.order_eqs

(* Run it *)
let () =
  let open Alcotest in
  run "Utils" [("string-case", [test_case "Lower case" `Quick test_lowercase])] *)
