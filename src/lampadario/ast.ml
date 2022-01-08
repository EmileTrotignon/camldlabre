open Common

(* lampadario is the highest level intermediate langage. It can be directly
   built from an OCaml AST *)

type ident = string

type node = {args: ident list; equations: expr String.Map.t; return: ident}

and expr =
  | EIf of expr * expr * expr
  | EVar of ident
  | ENotStream of Parsetree.expression
  | EFby of expr * expr
  | EPre of ident
  | EApp of expr * expr list
