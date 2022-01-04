type primitive = Primitive.t

type pattern =
  | PWildcard
  | PChar of char
  | PString of string
  | PInt of int
  | PTuple of pattern list
  | PVar of string
  | PCons of string * pattern list
  | PPrimitive of primitive

type type_ =
  | TName of string
  | TPrimitive of primitive
  | TApp of type_ list * string
  | TModuleField of string list
  | TArrow of type_ list * type_

type expr =
  | EIf of expr * expr * expr
  | ELet of (pattern * expr) list * expr
  | EApp of expr * (string * expr) list * expr list
  | ECons of string * expr list
  | EVar of string
  | ETuple of expr list
  | ELitList of expr list
  | ELitInt of int
  | ELitString of string
  | EFun of pattern list * expr
  | EPrimitive of primitive
  | ESequence of expr list * expr
  | EOpenModule of string * expr
  | EModuleField of string list
  | EMixedSequence of mixed list * expr
  | EMatch of expr * branch list
  | EUnit
  | EAnnotated of expr * type_

and branch = pattern * expr

and mixed = MiUnit of expr | MiPrimitive of primitive

type struct_item =
  | SIDef of pattern * expr
  | SIModule of string * module_
  | SIRecDefs of (pattern * expr) list

and struct_ = struct_item list

and module_ = MStruct of struct_ | MAlias of string | MField of string list
