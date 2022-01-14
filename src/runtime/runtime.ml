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

let apply f arg =
  match f with
  | UnInit ->
      UnInit
  | Skipped ->
      Skipped
  | Value f ->
      Value (f arg)

let iter f = function UnInit -> () | Skipped -> () | Value v -> f v

let to_string ~f = function UnInit -> "." | Skipped -> "_" | Value v -> f v

let print ~f = function
  | UnInit ->
      print_string "."
  | Skipped ->
      print_string "_"
  | Value v ->
      f v

let join frame =
  match frame with
  | UnInit ->
      UnInit
  | Skipped ->
      Skipped
  | Value frame -> (
    match frame with
    | UnInit ->
        UnInit
    | Skipped ->
        Skipped
    | Value v ->
        Value v )

let has_value frame =
  match frame with UnInit | Skipped -> false | Value _ -> true

let value ~default = function UnInit | Skipped -> default | Value v -> v

type 'a stream = unit -> 'a frame

let fby (s1 : 'a stream) (s2 : 'a stream) =
  let first = ref true in
  fun () ->
    if !first then (
      first := false ;
      s1 () )
    else s2 ()
