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

let compile_expr = function
  | EIf (cond, e1, e2) ->
      let ident = fresh_ident () in
      frame_bind ident (e_var cond) (e_if (e_var ident) (e_var e1) (e_var e2))
  | ESimple e -> (
    match e with
    | EVar ident ->
        e_cons "Runtime.Value" ~payload:[e_var ident]
    | ENotStream code ->
        e_cons "Runtime.Value"
          ~payload:
            [e_fun ([p_cons "()"] ^-> e_prim (Mocaml.Primitive.Parsed code))] )
  | EApplyNoStream (func, args) -> (
    match args with
    | [] ->
        assert false
    | arg :: args ->
        let func = e_prim (Mocaml.Primitive.Parsed func) in
        List.fold_left
          (fun expr arg -> frame_bind arg (e_var arg) expr)
          (frame_bind arg (e_var arg)
             (e_cons "Runtime.Value"
                ~payload:[e_app func ((args |> List.map e_var) @ [e_unit])] ) )
          args )
  | EApply (func, args) -> (
    match List.rev args with
    | [] ->
        assert false
    | arg :: args ->
        frame_bind func (e_var func)
          (List.fold_left
             (fun expr arg -> frame_bind arg (e_var arg) expr)
             (frame_map arg (e_var arg)
                (e_app (e_var func)
                   ((arg :: args |> List.rev |> List.map e_var) @ [e_unit]) ) )
             args ) )

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
  e_fun
  @@ ((args |> List.map p_var) @ [p_cons "()"])
  ^-> e_let (local_var |> List.map defref_uninit)
  @@ e_let (args |> List.map defcall_stream)
  @@ e_sequence
       ( assignments
       |> List.map (fun (ident, expr) ->
              e_let (derefs |> List.map compile_deref)
              @@ e_assign_to_ref (e_var ident) (compile_expr expr) ) )
  @@ e_sequence
       ( precedents
       |> List.map (fun {stream; var} ->
              e_assign_to_ref (e_var stream) (e_var var) ) )
  @@ e_deref @@ e_var return

let equal (a : unit) = ( = ) a
