type 'a frame = UnInit | Skipped | Value of 'a

let map ~f = function
  | UnInit ->
      UnInit
  | Skipped ->
      Skipped
  | Value v ->
      Value (f v)

let bind ~f = function
  | UnInit ->
      UnInit
  | Skipped ->
      Skipped
  | Value v -> (
    match f v with UnInit -> UnInit | Skipped -> Skipped | Value v -> Value v )

let value ~default = function UnInit | Skipped -> default | Value v -> v

type 'a stream = unit -> 'a frame
