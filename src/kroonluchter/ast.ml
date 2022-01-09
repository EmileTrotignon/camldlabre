type ident = string

(* Kroonluchter is the next intermediate langage, after lamapadario.
   The differences are :
   - [equations] become [assignments] : they have been sorted
   - There is an explicit difference between references and regular variables. *)

type simple_expr = EVar of ident | ENotStream of Parsetree.expression

type expr =
  | EIf of simple_expr * simple_expr * simple_expr
  | ESimple of simple_expr
  | EPre of ident
  | EApply of simple_expr * simple_expr list
  | EApplyNoStream of Parsetree.expression * simple_expr list

type node =
  { args: ident list
  ; local_var: ident list
  ; assignments: (ident * expr) list
  ; return: ident }
