type ident = string

type deref = {stream: ident; var: ident}

type node =
  { args: ident list
  ; local_var: ident list
  ; assignements: (ident * expr) list
  ; return: ident }

and expr =
  | EIf of expr * expr * expr
  | EVar of ident
  | ENotStream of Parsetree.expression
  | EFby of expr * expr
  | EPre of ident
  | EApp of expr * expr list
