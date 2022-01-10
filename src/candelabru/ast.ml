(* Candelabru is the simplest intermediate langage, it can be easily compiled to
   working OCaml code.
   The differences from Kroonluchter are :
   - [pre] is erased.
   - There are no references anymore, they have been replaced by an access to a
      derefenced variable. *)

type ident = string

type deref = {stream: ident; var: ident}

type simple_expr =
  | EVar of ident
  | EDeref of ident
  | ENotStream of Parsetree.expression

type expr =
  | EIf of simple_expr * simple_expr * simple_expr
  | ESimple of bool * simple_expr
  | EApply of simple_expr * simple_expr list
  | EApplyNoStream of Parsetree.expression * simple_expr list

type node =
  { args: ident list
  ; local_var: ident list
  ; precedents: deref list
  ; assignments: (ident * expr) list
  ; return: ident }
