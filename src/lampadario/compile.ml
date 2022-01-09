open Common
module S = Ast
module T = Kroonluchter.Ast

let fresh_ident = fresh_ident "__lampadario_fv_"

let rec compile_expr args (eqs : 'a String.Map.t) expr =
  let compile_expr = compile_expr args in
  match expr with
  | S.EIf (cond, e1, e2) ->
      let icond = fresh_ident ()
      and i1 = fresh_ident ()
      and i2 = fresh_ident () in
      let eqs, cond = compile_expr eqs cond in
      let eqs = String.Map.add icond cond eqs in
      let eqs, e1 = compile_expr eqs e1 in
      let eqs = String.Map.add i1 e1 eqs in
      let eqs, e2 = compile_expr eqs e2 in
      let eqs = String.Map.add i2 e2 eqs in
      (eqs, T.EIf (icond, i1, i2))
  | S.EVar ident ->
      (eqs, T.EVar ident) (* todo : handle arguments *)
  | S.ENotStream code ->
      (eqs, T.ENotStream code)
  | S.EApply (func, args) ->
      let ifunc = fresh_ident () in
      let eqs, func = compile_expr eqs func in
      let eqs = String.Map.add ifunc func eqs in
      let eqs, iargs =
        List.fold_left_map
          (fun eqs arg ->
            let iarg = fresh_ident () in
            let eqs, arg = compile_expr eqs arg in
            let eqs = String.Map.add iarg arg eqs in
            (eqs, iarg) )
          eqs args
      in
      (eqs, T.EApply (ifunc, iargs))
  | S.EApplyNoStream (func, args) ->
      let eqs, iargs =
        List.fold_left_map
          (fun eqs arg ->
            let iarg = fresh_ident () in
            let eqs, arg = compile_expr eqs arg in
            let eqs = String.Map.add iarg arg eqs in
            (eqs, iarg) )
          eqs args
      in
      (eqs, T.EApplyNoStream (func, iargs))
  | S.EPre ident ->
      (eqs, T.EPre ident)

let compile_equation args name expr eqs =
  let eqs, expr = compile_expr args eqs expr in
  String.Map.add name expr eqs

let fv_expr domain expr =
  let r =
    match expr with
    | T.EIf (cond, e1, e2) ->
        String.Set.of_list [cond; e1; e2]
    | T.EVar ident ->
        String.Set.singleton ident
    | T.ENotStream _code ->
        String.Set.empty
    | T.EApply (func, args) ->
        String.Set.add func (String.Set.of_list args)
    | T.EApplyNoStream (_func, args) ->
        String.Set.of_list args
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
  let equations =
    String.Map.fold (compile_equation args) equations String.Map.empty
  in
  let left_hand_side = String.Map.domain equations in
  let local_var = String.Set.elements left_hand_side in
  printf "nb local_var : %d\n" (List.length local_var) ;
  let assignments = order_eqs left_hand_side equations in
  T.{args; local_var; assignments; return}

let equal (a : unit) = ( = ) a
