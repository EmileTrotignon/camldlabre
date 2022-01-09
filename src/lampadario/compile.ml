open Common
module S = Ast
module T = Kroonluchter.Ast

let fresh_ident = fresh_ident "__kroonluchter_fv_"

let rec compile_expr streams expr =
  let ce = compile_expr streams in
  match expr with
  | S.EIf (cond, e1, e2) ->
      let cond = ce cond in
      T.EIf (cond, ce e1, ce e2)
  | S.EVar ident ->
      if String.Set.mem ident streams then T.EDeRef ident
      else T.EVar ident (* todo false *)
  | S.ENotStream code ->
      T.ENotStream code
  | S.EApply (func, args) ->
      T.EApply (ce func, List.map ce args)
  | S.EApplyNoStream (func, args) ->
      T.EApplyNoStream (ce func, List.map ce args)
  | S.EPre e ->
      T.EPre e

let rec fv_expr domain expr =
  let fv_expr = fv_expr domain in
  let r =
    match expr with
    | S.EIf (cond, e1, e2) ->
        String.Set.unions [fv_expr cond; fv_expr e1; fv_expr e2]
    | S.EVar ident ->
        String.Set.singleton ident
    | S.ENotStream _code ->
        String.Set.empty
    | S.EApply (func, args) | S.EApplyNoStream (func, args) ->
        String.Set.union (fv_expr func)
          (args |> List.map fv_expr |> String.Set.unions)
    | S.EPre _e ->
        String.Set.empty
  in
  String.Set.inter domain r

let order_eqs domain eqs =
  (* Equations without dependences are first*)
  let deps = String.Map.map (fv_expr domain) eqs in
  let deps_G = String.Graph.of_map deps in
  assert (not @@ String.Graph.Dfs.has_cycle deps_G) ;
  printf "nb vertex : %d\n" (String.Graph.nb_vertex deps_G) ;
  String.Graph.Topological.fold List.cons deps_G []
  |> List.map (fun ident -> (ident, String.Map.find ident eqs))

let compile_node S.{args; equations; return} =
  printf "n equations = %d\n" (String.Map.size equations) ;
  let left_hand_side = String.Map.domain equations in
  let local_var = String.Set.elements left_hand_side in
  let assignments =
    equations |> order_eqs left_hand_side
    |> List.map (fun (ident, expr) -> (ident, compile_expr left_hand_side expr))
  in
  T.{args; local_var; assignments; return}

let equal (a : unit) = ( = ) a
