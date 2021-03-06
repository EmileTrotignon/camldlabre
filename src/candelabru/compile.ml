open Common
open Ast
open Mocaml.Builder

let fresh_ident = fresh_ident "__candelabru_fv_"

let bind module_ name frame body =
  e_app (e_module_field [module_; "bind"]) [e_fun ([p_var name], body); frame]

let map module_ name frame body =
  e_app (e_module_field [module_; "map"]) [e_fun ([p_var name], body); frame]

let fun_unit body = e_fun ([p_cons "()"] ^-> body)

let frame_value option default =
  e_app
    (e_module_field ["Runtime"; "value"])
    ~named_args:[("default", default)]
    [option]

let frame_bind = bind "Runtime"

let frame_map = map "Runtime"

let frame_gmap func frame =
  e_app (e_module_field ["Runtime"; "map"]) [func; frame]

let frame_apply func arg =
  e_app (e_module_field ["Runtime"; "apply"]) [func; arg]

let frame_join frame = e_app (e_module_field ["Runtime"; "join"]) [frame]

let framed e = e_cons "Runtime.Value" ~payload:[e]

let framed_if (frame_ident, expr) =
  if Option.is_some frame_ident then expr else framed expr

let compile_expr =
  let compile_simple = function
    | EVar ident ->
        (None, e_var ident)
    | EDeref ident ->
        (Some ident, e_deref (e_var ident))
    | ENotStream e ->
        (None, framed @@ e_prim (Mocaml.Primitive.Parsed e))
  in
  let _framed_simple e = framed_if (compile_simple e) in
  let compile_apply func_is_frame func arg =
    let apply =
      if func_is_frame then frame_apply else fun func arg -> e_app func [arg]
    in
    let arg =
      match arg with
      | EVar ident ->
          e_var ident
      | ENotStream expr ->
          fun_unit (framed @@ e_prim (Mocaml.Primitive.Parsed expr))
      | EDeref ident ->
          fun_unit (e_deref (e_var ident))
    in
    apply func arg
  in
  function
  | EIf (cond, e1, e2) -> (
      ( []
      , let frame_ident_cond, cond = compile_simple cond in
        let _, e1 = compile_simple e1 in
        let _, e2 = compile_simple e2 in
        match frame_ident_cond with
        | Some ident ->
            frame_bind ident cond (e_if (e_var ident) e1 e2)
        | None ->
            e_if cond e1 e2 ) )
  | ESimple (is_pre, e) -> (
      ( []
      , match e with
        | EVar ident ->
            let e = e_var ident in
            if is_pre then e else framed e
        | EDeref ident ->
            e_deref (e_var ident)
        | ENotStream e ->
            framed @@ e_prim (Mocaml.Primitive.Parsed e) ) )
  | EApplyNoStream (func, args) -> (
      ( []
      , match args with
        | [] ->
            assert false
        | arg :: args ->
            let func = e_prim (Mocaml.Primitive.Parsed func) in
            let get_bound = function
              | EVar ident | EDeref ident ->
                  e_var ident
              | ENotStream e ->
                  e_prim (Mocaml.Primitive.Parsed e)
            in
            let bind_arg map expr arg =
              let bind = if map then frame_map else frame_bind in
              match arg with
              | EVar ident ->
                  (false, bind ident (e_var ident) expr)
              | EDeref ident ->
                  (false, bind ident (e_deref (e_var ident)) expr)
              | ENotStream _e ->
                  (map, expr)
            in
            let unmapped, expr =
              List.fold_left
                (fun (map, expr) arg -> bind_arg map expr arg)
                (bind_arg true
                   (e_app func (arg :: args |> List.map get_bound))
                   arg )
                args
            in
            if unmapped then framed expr else expr ) )
  | EApply (func, args) -> (
    match args with
    | [] ->
        assert false
    | arg :: args ->
        let func_application =
          List.fold_left (compile_apply false)
            (let _func_frame_ident, func = compile_simple func in
             compile_apply false func arg )
            args
        in
        let func_app_ident = fresh_ident () in
        ( [p_var func_app_ident ^= func_application]
        , e_app (e_var func_app_ident) [e_unit] ) )

let defref_uninit local_var = p_var local_var ^= e_ref (e_cons "Runtime.UnInit")

let arg_name = sprintf "__arg_%s"

let compile_deref {stream; var} = p_var var ^= e_deref (e_var stream)

let compile_node
    { args: ident list
    ; local_var: ident list
    ; precedents: deref list
    ; assignments: (ident * expr) list
    ; return: ident } =
  let assignements_arg =
    args
    |> List.map (fun arg ->
           e_assign_to_ref (e_var arg)
             (e_app (arg |> arg_name |> e_var) [e_unit]) )
  in
  let def_applies, assignments =
    assignments
    |> List.fold_left_map
         (fun defs_acc (ident, expr) ->
           let defs, expr = compile_expr expr in
           (defs @ defs_acc, e_assign_to_ref (e_var ident) expr) )
         []
  in
  e_fun
  @@ (args |> List.map arg_name |> List.map p_var)
  ^-> e_let (local_var |> List.map defref_uninit)
  @@ e_let (args |> List.map defref_uninit)
  @@ e_let def_applies @@ e_fun
  @@ [p_cons "()"]
  ^-> e_let (precedents |> List.map compile_deref)
  @@ e_sequence assignements_arg
  @@ e_sequence assignments
  (*@@ e_sequence
       ( precedents
       |> List.map (fun {stream; var} ->
              e_assign_to_ref (e_var stream) (e_var var) ) )*)
  @@ e_deref
  @@ e_var return

let equal (a : unit) = ( = ) a
