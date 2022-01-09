open Ast
open Mocaml.Builder

let fresh_ident = Common.fresh_ident "__candelabru_fv_"

let bind module_ name frame body =
  e_app (e_module_field [module_; "bind"]) [e_fun ([p_var name], body); frame]

let map module_ name frame body =
  e_app (e_module_field [module_; "map"]) [e_fun ([p_var name], body); frame]

let frame_value option default =
  e_app
    (e_module_field ["Runtime"; "value"])
    ~named_args:[("default", default)]
    [option]

let frame_bind = bind "Runtime"

let frame_map = map "Runtime"

let framed e = e_cons "Runtime.Value" ~payload:[e]

let framed_if (frame_ident, expr) =
  if Option.is_some frame_ident then expr else framed expr

let compile_expr frames =
  let compile_simple = function
    | EVar ident ->
        ((if List.mem ident frames then Some ident else None), e_var ident)
    | ENotStream e ->
        ( None
        , e_fun ([p_cons "()"] ^-> framed @@ e_prim (Mocaml.Primitive.Parsed e))
        )
  in
  let framed_simple e = framed_if (compile_simple e) in
  function
  | EIf (cond, e1, e2) -> (
      ( []
      , let frame_ident_cond, cond = compile_simple cond in
        let e1 = framed_simple e1 in
        let e2 = framed_simple e2 in
        match frame_ident_cond with
        | Some ident ->
            frame_bind ident cond (e_if (e_var ident) e1 e2)
        | None ->
            e_if cond e1 e2 ) )
  | ESimple e ->
      ([], framed_if (compile_simple e))
  | EApplyNoStream (func, args) -> (
      ( []
      , match args with
        | [] ->
            assert false
        | arg :: args ->
            let func = e_prim (Mocaml.Primitive.Parsed func) in
            List.fold_left
              (fun expr arg ->
                let arg_ident = fresh_ident () in
                frame_bind arg_ident (framed_simple arg) expr )
              (let arg_ident = fresh_ident () in
               frame_map arg_ident (framed_simple arg)
                 (e_app func
                    ( (arg :: args |> List.rev |> List.map framed_simple)
                    @ [e_unit] ) ) )
              args ) )
  | EApply (func, args) -> (
      let original_args = args in
      match List.rev args with
      | [] ->
          assert false
      | _arg :: _args ->
          let func_app =
            let func_frame_ident, func = compile_simple func in
            match func_frame_ident with
            | Some _ident ->
                failwith "todo"
            | None ->
                e_app func
                  (original_args |> List.map compile_simple |> List.map snd)
          in
          let func_app_ident = fresh_ident () in
          ( [p_var func_app_ident ^= func_app]
          , e_app (e_var func_app_ident) [e_unit] ) )

let defref_uninit local_var = p_var local_var ^= e_ref (e_cons "Runtime.UnInit")

let defcall_stream stream = p_var stream ^= e_app (e_var stream) [e_unit]

let compile_deref {stream; var} = p_var var ^= e_deref (e_var stream)

let compile_node
    { args: ident list
    ; local_var: ident list
    ; derefs: deref list
    ; precedents: deref list
    ; assignments: (ident * expr) list
    ; return: ident } =
  let frames = List.map (fun {stream= _; var} -> var) derefs in
  let def_applies, assignments =
    assignments
    |> List.fold_left_map
         (fun defs_acc (ident, expr) ->
           let defs, expr = compile_expr frames expr in
           ( defs @ defs_acc
           , e_let (derefs |> List.map compile_deref)
             @@ e_assign_to_ref (e_var ident) expr ) )
         []
  in
  e_fun
  @@ (args |> List.map p_var)
  ^-> e_let (local_var |> List.map defref_uninit)
  @@ e_let (args |> List.map defcall_stream)
  @@ e_let def_applies @@ e_fun
  @@ [p_cons "()"]
  ^-> e_sequence assignments
  @@ e_sequence
       ( precedents
       |> List.map (fun {stream; var} ->
              e_assign_to_ref (e_var stream) (e_var var) ) )
  @@ e_deref @@ e_var return

let equal (a : unit) = ( = ) a
