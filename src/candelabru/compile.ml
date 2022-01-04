open Ast
open Mocaml.Builder

let fresh_ident = Common.fresh_ident "__candelabru_fv_"

let bind module_ name o body =
  e_app (e_module_field [module_; "bind"]) [o; e_fun ([p_var name], body)]

let map module_ f o = e_app (e_module_field [module_; "map"]) [f; o]

let frame_value option default =
  e_app
    (e_module_field ["Runtime"; "value"])
    ~named_args:[("default", default)] [option]

let frame_bind = bind "Runtime"

let frame_map = map "Runtime"

let rec compile_expr = function
  | EIf (cond, e1, e2) ->
      let ident = fresh_ident () in
      frame_bind ident (compile_expr cond)
        (e_if (e_var ident) (compile_expr e1) (compile_expr e2))
  | EVar ident ->
      e_var ident
  | ENotStream code ->
      e_prim (Mocaml.Primitive.Parsed code)
  | EIfUnInit (cond, e1, e2) ->
      e_match (compile_expr cond)
        [ p_cons "Runtime.UnInit" ^-> compile_expr e1
        ; p_cons ~payload:[p_wildcard] "Runtime.UnInit" ^-> compile_expr e2 ]
  | EApp (func, args) -> (
    match args with
    | [] ->
        assert false
    | x :: xs ->
        let func = compile_expr func in
        let x = compile_expr x in
        List.fold_left
          (fun func arg -> frame_map func (compile_expr arg))
          (frame_map func x) xs )

let defref_none local_var = p_var local_var ^= e_ref (e_var "None")

let defcall_stream stream = p_var stream ^= e_app (e_var stream) [e_unit]

let compile_deref {stream; var} = p_var var ^= e_deref (e_var stream)

let compile_node
    { args: ident list
    ; local_var: ident list
    ; derefs: deref list
    ; precedents: deref list
    ; assignements: (ident * expr) list
    ; return: ident } =
  e_fun
  @@ (args |> List.map p_var)
  ^-> e_let (local_var |> List.map defref_none)
  @@ e_let (args |> List.map defcall_stream)
  @@ e_sequence
       ( assignements
       |> List.map (fun (ident, expr) ->
              e_let (derefs |> List.map compile_deref)
              @@ e_assign_to_ref (e_var ident) (compile_expr expr) ) )
  @@ e_sequence
       ( precedents
       |> List.map (fun {stream; var} ->
              e_assign_to_ref (e_var stream) (e_var var) ) )
  @@ e_var return

let equal (a : unit) = ( = ) a
