module S = Ast
module T = Candelabru.Ast

let fresh_ident = Common.fresh_ident "__kroonluchter_fv_"

let rec compile_expr derefs = function
  | S.EIf (cond, e1, e2) ->
      let derefs, cond = compile_expr derefs cond in
      let derefs, e1 = compile_expr derefs e1 in
      let derefs, e2 = compile_expr derefs e2 in
      (derefs, T.EIf (cond, e1, e2))
  | S.EVar ident ->
      (derefs, T.EVar ident)
  | S.ENotStream code ->
      (derefs, T.ENotStream code)
  | S.EApp (func, args) ->
      let derefs, func = compile_expr derefs func in
      let derefs, args = List.fold_left_map compile_expr derefs args in
      (derefs, T.EApp (func, args))
  | S.EFby (first, then_) ->
      let derefs, first = compile_expr derefs first in
      let derefs, then_ = compile_expr derefs then_ in
      (derefs, T.EIfUnInit (then_, first, then_))
  | S.EPre _e ->
      failwith "TODO"
  | EDeRef stream -> (
    match List.assoc_opt stream derefs with
    | None ->
        let var = fresh_ident () in
        let derefs = (stream, var) :: derefs in
        (derefs, EVar var)
    | Some var ->
        (derefs, EVar var) )

let compile_node
    S.
      { args: ident list
      ; local_var: ident list
      ; assignments: (ident * expr) list
      ; return: ident } =
  let precedents = [] in
  let derefs, assignments =
    List.fold_left_map
      (fun derefs (ident, expr) ->
        let derefs, nexpr = compile_expr derefs expr in
        (derefs, (ident, nexpr)) )
      [] assignments
  in
  let derefs = derefs |> List.map (fun (stream, var) -> T.{stream; var}) in
  T.{args; local_var; derefs; precedents; assignments; return}

let equal (a : unit) = ( = ) a
