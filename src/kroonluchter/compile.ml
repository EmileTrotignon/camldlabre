open Common

module S = Ast
module T = Candelabru.Ast

let fresh_ident = fresh_ident "__kroonluchter_fv_"

let rec compile_expr (precedents, derefs) = function
  | S.EIf (cond, e1, e2) ->
      let (precedents, derefs), cond = compile_expr (precedents, derefs) cond in
      let (precedents, derefs), e1 = compile_expr (precedents, derefs) e1 in
      let (precedents, derefs), e2 = compile_expr (precedents, derefs) e2 in
      ((precedents, derefs), T.EIf (cond, e1, e2))
  | S.EVar ident ->
      ((precedents, derefs), T.EVar ident)
  | S.ENotStream code ->
      ((precedents, derefs), T.ENotStream code)
  | S.EApply (func, args) ->
      let (precedents, derefs), func = compile_expr (precedents, derefs) func in
      let (precedents, derefs), args =
        List.fold_left_map compile_expr (precedents, derefs) args
      in
      ((precedents, derefs), T.EApply (func, args))
  | S.EApplyNoStream (func, args) ->
      let (precedents, derefs), func = compile_expr (precedents, derefs) func in
      let (precedents, derefs), args =
        List.fold_left_map compile_expr (precedents, derefs) args
      in
      ((precedents, derefs), T.EApplyNoStream (func, args))
  | S.EPre stream -> (
    match List.assoc_opt stream precedents with
    | None ->
        let var = fresh_ident () in
        let precedents = (stream, var) :: precedents in
        ((precedents, derefs), EVar var)
    | Some var ->
        ((precedents, derefs), EVar var) )
  | EDeRef stream -> (
    match List.assoc_opt stream derefs with
    | None ->
        let var = fresh_ident () in
        let derefs = (stream, var) :: derefs in
        ((precedents, derefs), EVar var)
    | Some var ->
        ((precedents, derefs), EVar var) )

let compile_node
    S.
      { args: ident list
      ; local_var: ident list
      ; assignments: (ident * expr) list
      ; return: ident } =
  printf "n assign = %d\n" (List.length assignments) ;
  let (precedents, derefs), assignments =
    List.fold_left_map
      (fun (precedents, derefs) (ident, expr) ->
        let (precedents, derefs), nexpr =
          compile_expr (precedents, derefs) expr
        in
        ((precedents, derefs), (ident, nexpr)) )
      ([], []) assignments
  in
  let derefs = derefs |> List.map (fun (stream, var) -> T.{stream; var}) in
  let precedents =
    precedents |> List.map (fun (stream, var) -> T.{stream; var})
  in
  T.{args; local_var; derefs; precedents; assignments; return}

let equal (a : unit) = ( = ) a
