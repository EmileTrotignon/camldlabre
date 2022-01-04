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
  | S.EApp (_func, _args) ->
      failwith "TODO"
  | S.EFby (first, then_) ->
      failwith "TODO"
  | S.EPre e ->
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
      ; assignements: (ident * expr) list
      ; return: ident } =
  let precedents = [] in
  let derefs, assignements =
    List.fold_left_map
      (fun derefs (ident, expr) ->
        let derefs, nexpr = compile_expr derefs expr in
        (derefs, (ident, nexpr)) )
      [] assignements
  in
  let derefs = derefs |> List.map (fun (stream, var) -> T.{stream; var}) in
  T.{args; local_var; derefs; precedents; assignements; return}

let equal (a : unit) = ( = ) a
