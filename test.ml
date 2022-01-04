(* syntaxe lustre *)
(* node STABLE (set: bool; delay: int) returns (level: bool);
   var count: int;
   let
     level = (count>0);
     count = if set then delay
             else if false->pre(level)
                  then pre(count) - 1
                  else 0;
   tel *)

(* syntaxe ocaml utilisateur *)
let%node stable set delay =
  let level = count > 0
  and count =
    if set then delay
    else if [%notstream false] => pre level then
      [%notstream ( - )] (pre count) [%notstream 1]
      (* todo : [%notstream [%stream pre count] - 1] *)
    else 0
  in
  level

(* code ocaml produit *)
type 'a stream = unit -> 'a option

let stable (set: bool stream) (delay: int stream) () =
  let s_count = ref None
  and s_level = ref None
  and s_prelevel = ref None
  and s_precount = ref None in

  let precount = !s_count and prelevel = !s_level in
  (* input updating *)
  let delay = delay () and set = set () in
  (* stable *)
  s_count :=
    Option.bind set (fun set ->
        if set then delay
        else if Option.value ~default:false !s_prelevel then
          !s_precount |> Option.map (( - ) 1)
        else Some 0 ) ;
  s_level := !s_count |> Option.map (fun count -> count > 0) ;
  (* end stable *)
  s_precount := precount ;
  s_prelevel := prelevel ;
  !s_level

type ident = string

type deref = {stream:ident; var:ident}

type node =
  { arg: ident list
  ; local_var: ident list
  ; derefs : deref list
  ; vars_to_put_aside: (ident * ident) list
  ; assignements: (ident * expr) list
  ; return: ident }

and expr =
  | EIf of expr * expr * expr
  | EVar of ident
  | ENotStream of string
  | EIfNone of expr * expr
  | EApp of expr * expr list
