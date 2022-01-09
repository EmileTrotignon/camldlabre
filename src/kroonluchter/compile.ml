open Common
module S = Ast
module T = Candelabru.Ast

let fresh_ident = fresh_ident "__kroonluchter_fv_"

let handle_ident arguments local_var (precedents, derefs) ident =
  if List.mem ident local_var || List.mem ident arguments then (
    print_endline "coucou 4" ;
    match List.assoc_opt ident derefs with
    | None ->
        let var = fresh_ident () in
        let derefs = (ident, var) :: derefs in
        ((precedents, derefs), var)
    | Some var ->
        ((precedents, derefs), var) )
  else ((precedents, derefs), ident)

let compile_sexpr arguments local_var (precedents, derefs) =
  let handle_ident = handle_ident arguments local_var in
  function
  | S.EVar ident ->
      let (precedents, derefs), ident =
        handle_ident (precedents, derefs) ident
      in
      ((precedents, derefs), T.EVar ident)
  | S.ENotStream code ->
      ((precedents, derefs), T.ENotStream code)

let compile_expr arguments local_var (precedents, derefs) =
  let handle_ident = handle_ident arguments local_var in
  function
  | S.EIf (cond, e1, e2) ->
      let (precedents, derefs), cond = handle_ident (precedents, derefs) cond in
      let (precedents, derefs), e1 = handle_ident (precedents, derefs) e1 in
      let (precedents, derefs), e2 = handle_ident (precedents, derefs) e2 in
      ((precedents, derefs), T.EIf (cond, e1, e2))
  | S.ESimple e ->
      let (precedents, derefs), e = handle_ident (precedents, derefs) e in
      ((precedents, derefs), T.ESimple e)
  | S.EApply (func, args) ->
      let (precedents, derefs), func = handle_ident (precedents, derefs) func in
      let (precedents, derefs), args =
        List.fold_left_map handle_ident (precedents, derefs) args
      in
      ((precedents, derefs), T.EApply (func, args))
  | S.EApplyNoStream (func, args) ->
      let (precedents, derefs), args =
        List.fold_left_map handle_ident (precedents, derefs) args
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

let compile_node
    S.
      { args: ident list
      ; local_var: ident list
      ; assignments: (ident * expr) list
      ; return: ident } =
  let (precedents, derefs), assignments =
    List.fold_left_map
      (fun (precedents, derefs) (ident, expr) ->
        let (precedents, derefs), nexpr =
          compile_expr args local_var (precedents, derefs) expr
        in
        ((precedents, derefs), (ident, nexpr)) )
      ([], []) assignments
  in
  printf "nb derefs = %d\n" (List.length derefs) ;
  let derefs = derefs |> List.map (fun (stream, var) -> T.{stream; var}) in
  let precedents =
    precedents |> List.map (fun (stream, var) -> T.{stream; var})
  in
  T.{args; local_var; derefs; precedents; assignments; return}

let equal (a : unit) = ( = ) a
