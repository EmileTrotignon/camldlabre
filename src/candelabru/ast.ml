
(* Candelabru is the simplest intermediate langage, it can be easily compiled to
   working OCaml code.
   The differences from Kroonluchter are :
   - [pre] is erased.
   - There are no references anymore, they have been replaced by an access to a
      derefenced variable. *)

type ident = string

type deref = {stream: ident; var: ident}

type node =
  { args: ident list
  ; local_var: ident list
  ; derefs: deref list
  ; precedents: deref list
  ; assignments: (ident * expr) list
  ; return: ident }

and expr =
  | EIf of expr * expr * expr
  | EVar of ident
  | ENotStream of Parsetree.expression
  | EIfUnInit of expr * expr * expr
  | EApply of expr * expr list
  | EApplyNoStream of expr * expr list
