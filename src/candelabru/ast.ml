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
  | EApp of expr * expr list