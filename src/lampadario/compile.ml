open Common
module S = Ast
module T = Kroonluchter.Ast

let fresh_ident = fresh_ident "__lampadario_fv_"

let ident_of_expr = function
  | T.ESimple s ->
      `Simple s
  | _ ->
      let ident = fresh_ident () in
      `Need_eq (ident, T.EVar ident)

let bind = String.Map.add

let bind_if bool key data map = if bool then bind key data map else map

let rec compile_subexpr eqs expr =
  match expr with
  | S.EVar ident ->
      (eqs, T.EVar ident)
  | S.ENotStream prim ->
      (eqs, T.ENotStream prim)
  | _ ->
      let iexpr = fresh_ident () in
      let eqs, expr = compile_expr eqs expr in
      let eqs = String.Map.add iexpr expr eqs in
      (eqs, T.EVar iexpr)

and compile_expr (eqs : 'a String.Map.t) expr =
  match expr with
  | S.EIf (cond, e1, e2) ->
      let eqs, cond = compile_subexpr eqs cond in
      let eqs, e1 = compile_subexpr eqs e1 in
      let eqs, e2 = compile_subexpr eqs e2 in
      (eqs, T.EIf (cond, e1, e2))
  | S.EVar ident ->
      (eqs, T.ESimple (T.EVar ident))
  | S.ENotStream code ->
      (eqs, T.ESimple (T.ENotStream code))
  | S.EApply (func, args) ->
      let eqs, func = compile_subexpr eqs func in
      let eqs, args = List.fold_left_map compile_subexpr eqs args in
      (eqs, T.EApply (func, args))
  | S.EApplyNoStream (func, args) ->
      let eqs, args = List.fold_left_map compile_subexpr eqs args in
      (eqs, T.EApplyNoStream (func, args))
  | S.EPre ident ->
      (eqs, T.EPre ident)

let compile_equation name expr eqs =
  let eqs, expr = compile_expr eqs expr in
  bind name expr eqs

let fv_simple = function
  | T.EVar ident ->
      String.Set.singleton ident
  | T.ENotStream _prim ->
      String.Set.empty

let fv_expr domain expr =
  let r =
    match expr with
    | T.EIf (cond, e1, e2) ->
        String.Set.unions [fv_simple cond; fv_simple e1; fv_simple e2]
    | T.ESimple (EVar ident) ->
        String.Set.singleton ident
    | T.ESimple (T.ENotStream _code) ->
        String.Set.empty
    | T.EApply (func, args) ->
        String.Set.unions (fv_simple func :: (args |> List.map fv_simple))
    | T.EApplyNoStream (_func, args) ->
        String.Set.unions (args |> List.map fv_simple)
    | T.EPre _e ->
        String.Set.empty
  in
  String.Set.inter domain r

let order_eqs domain eqs =
  (* Equations without dependences are first*)
  let deps = String.Map.map (fv_expr domain) eqs in
  let deps_G = String.Graph.of_map deps in
  assert (not @@ String.Graph.Dfs.has_cycle deps_G) ;
  printf "nb vertex : %d\n" (String.Graph.nb_vertex deps_G) ;
  let ( let* ) = Fun.flip Option.map in
  String.Graph.Topological.fold List.cons deps_G []
  |> List.filter_map (fun ident ->
         let* rhs = String.Map.find_opt ident eqs in
         (ident, rhs) )

let compile_node S.{args; equations; return} =
  printf "n equations = %d\n" (String.Map.size equations) ;
  let equations = String.Map.fold compile_equation equations String.Map.empty in
  let left_hand_side = String.Map.domain equations in
  let local_var = String.Set.elements left_hand_side in
  printf "nb local_var : %d\n" (List.length local_var) ;
  let assignments = order_eqs left_hand_side equations in
  T.{args; local_var; assignments; return}

let equal (a : unit) = ( = ) a
