open Common
module S = Ast
module T = Candelabru.Ast

let fresh_ident = fresh_ident "__kroonluchter_fv_"

let compile_sexpr arguments local_var = function
  | S.EVar ident ->
      if List.mem ident local_var || List.mem ident arguments then
        T.EDeref ident
      else T.EVar ident
  | S.ENotStream code ->
      T.ENotStream code

let compile_expr arguments local_var precedents =
  let compile_sexpr = compile_sexpr arguments local_var in
  function
  | S.EIf (cond, e1, e2) ->
      let cond = compile_sexpr cond in
      let e1 = compile_sexpr e1 in
      let e2 = compile_sexpr e2 in
      (precedents, T.EIf (cond, e1, e2))
  | S.ESimple e ->
      let e = compile_sexpr e in
      (precedents, T.ESimple (false, e))
  | S.EApply (func, args) ->
      let func = compile_sexpr func in
      let args = args |> List.map compile_sexpr in
      (precedents, T.EApply (func, args))
  | S.EApplyNoStream (func, args) ->
      let args = args |> List.map compile_sexpr in
      (precedents, T.EApplyNoStream (func, args))
  | S.EPre stream -> (
    match List.assoc_opt stream precedents with
    | None ->
        let var = sprintf "__pre_%s" stream in
        let precedents = (stream, var) :: precedents in
        (precedents, T.ESimple (true, EVar var))
    | Some var ->
        (precedents, T.ESimple (true, EVar var)) )

let compile_node
    S.
      { args: ident list
      ; local_var: ident list
      ; assignments: (ident * expr) list
      ; return: ident } =
  let precedents, assignments =
    List.fold_left_map
      (fun precedents (ident, expr) ->
        let precedents, nexpr = compile_expr args local_var precedents expr in
        (precedents, (ident, nexpr)) )
      [] assignments
  in
  let precedents =
    precedents |> List.map (fun (stream, var) -> T.{stream; var})
  in
  T.{args; local_var; precedents; assignments; return}

let equal (a : unit) = ( = ) a
