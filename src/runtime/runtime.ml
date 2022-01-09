type 'a frame = UnInit | Skipped | Value of 'a

let map f = function
  | UnInit ->
      UnInit
  | Skipped ->
      Skipped
  | Value v ->
      Value (f v)

let bind f = function
  | UnInit ->
      UnInit
  | Skipped ->
      Skipped
  | Value v -> (
    match f v with UnInit -> UnInit | Skipped -> Skipped | Value v -> Value v )

let has_value frame =
  match frame with UnInit | Skipped -> false | Value _ -> true

let value ~default = function UnInit | Skipped -> default | Value v -> v

type 'a stream = unit -> 'a frame

let fby s1 s2 =
  let first = ref true in
  fun () ->
    if !first then (
      first := false ;
      s1 () )
    else s2 ()
