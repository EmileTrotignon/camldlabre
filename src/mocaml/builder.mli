val ( ^-> ) : 'a -> 'b -> 'a * 'b

val ( ^= ) : 'a -> 'b -> 'a * 'b

open Ast

val t_name : string -> type_

val t_primitive : Primitive.t -> type_

val t_app : type_ list -> string -> type_

val t_module_field : string list -> type_

val t_arrow : type_ list * type_ -> type_

val e_if : expr -> expr -> expr -> expr

val e_let : (pattern * expr) list -> expr -> expr

val e_leti : (pattern * expr) list * expr -> expr

val e_app : expr -> ?named_args:(string * expr) list -> expr list -> expr

val e_cons : ?payload:expr list -> string -> expr

val e_var : string -> expr

val e_tuple : expr list -> expr

val e_lit_list : expr list -> expr

val e_lit_string : string -> expr

val e_lit_int : int -> expr

val e_fun : pattern list * expr -> expr

val e_prim : Primitive.t -> expr

val e_ref : expr -> expr

val e_deref : expr -> expr

val e_assign_to_ref : expr -> expr -> expr

val e_sequence : expr list -> expr -> expr

val e_open_module : string -> expr -> expr

val e_module_field : string list -> expr

val e_mixed_seq : mixed list -> expr -> expr

val e_match : expr -> branch list -> expr

val e_unit : expr

val e_annot : expr -> type_ -> expr

val ( ^: ) : expr -> type_ -> expr

val e_li_cons : expr -> expr -> expr

val e_empty_list : expr

val e_empty_string : expr

val e_function : branch list -> expr

val p_wildcard : pattern

val p_char : char -> pattern

val p_string : string -> pattern

val p_int : int -> pattern

val p_tuple : pattern list -> pattern

val p_var : string -> pattern

val p_prim : Primitive.t -> pattern

val p_cons : ?payload:pattern list -> string -> pattern

val mix_unit : expr -> mixed

val mix_prim : Primitive.t -> mixed

val si_def : pattern * expr -> struct_item

val si_module : string * module_ -> struct_item

val m_struct : struct_ -> module_

val m_alias : string -> module_

val m_field : string list -> module_

val prim_textual : string -> Lexing.position -> Lexing.position -> Primitive.t

val prim_parsed : Parsetree.expression -> Primitive.t
