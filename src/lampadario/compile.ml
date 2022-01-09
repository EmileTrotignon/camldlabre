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
      if String.Set.mem ident streams then T.EDeRef ident else T.EVar ident 
  | S.ENotStream code ->
      T.ENotStream code
  | S.EApp (func, args) ->
      T.EApp (ce func, List.map ce args)
  | S.EFby (first, then_) ->
      T.EFby (ce first, ce then_)
  | S.EPre e ->
      T.EPre e

(* A function that give the dependancies of an equation *)
let rec fv_expr expr =
  match expr with
  | S.EIf (cond, e1, e2) ->
      String.Set.unions [fv_expr cond; fv_expr e1; fv_expr e2]
  | S.EVar ident ->
      String.Set.singleton ident
  | S.ENotStream _code ->
      String.Set.empty
  | S.EApp (func, args) ->
      String.Set.union (fv_expr func)
        (args |> List.map fv_expr |> String.Set.unions)
  | S.EFby (first, then_) ->
      String.Set.union (fv_expr first) (fv_expr then_)
  | S.EPre _e ->
      String.Set.empty

(*A function that create a order for equations according to their dependancies*)
let order_eqs eqs =
  (* Equations without dependences are first*)
  let deps = String.Map.map fv_expr eqs in
  let deps_G = String.Graph.of_map deps in
  assert (not @@ String.Graph.Dfs.has_cycle deps_G) ;
  String.Graph.Topological.fold List.cons deps_G []
  |> List.map (fun ident -> (ident, String.Map.find ident eqs))

let compile_node S.{args; equations; return} =
  let left_hand_side = String.Map.domain equations in
  let local_var = String.Set.elements left_hand_side in
  let assignments =
    equations |> order_eqs
    |> List.map (fun (ident, expr) -> (ident, compile_expr left_hand_side expr))
  in
  T.{args; local_var; assignments; return}

let equal (a : unit) = ( = ) a
