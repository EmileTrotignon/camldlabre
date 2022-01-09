type ident = string

(* Kroonluchter is the next intermediate langage, after lamapadario.
   The differences are :
   - [equations] become [assignments] : they have been sorted
   - There is an explicit difference between references and regular variables. *)

type node =
  { args: ident list
  ; local_var: ident list
  ; assignments: (ident * expr) list
  ; return: ident }

and expr =
  | EIf of ident * ident * ident
  | EVar of ident
  | ENotStream of Parsetree.expression
  | EPre of ident
  | EApply of ident * ident list
  | EApplyNoStream of Parsetree.expression * ident list

