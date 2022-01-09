open Ppxlib
open Common

let name = "node"

let pattern_nostream p =
  Ast_pattern.(
    pexp_extension (extension (string "nostream") (single_expr_payload p)))

let pattern_nostream_apply p_f p_args =
  Ast_pattern.(
    pexp_extension
      (extension (string "nostream_apply")
         (single_expr_payload (pexp_apply p_f p_args)) ))

let rec pattern_lampadario_expr () =
  Ast_pattern.(
    let rec_parse expr =
      parse (pattern_lampadario_expr ()) expr.pexp_loc expr ()
    in
    map ~f:(fun _ ident -> Lampadario.Ast.EVar ident) (pexp_ident (lident __))
    ||| map
          ~f:(fun _ cond e1 e2 ->
            Lampadario.Ast.EIf (rec_parse cond, rec_parse e1, rec_parse e2) )
          (pexp_ifthenelse __ __ (some __))
    ||| map
          ~f:(fun _ ident -> Lampadario.Ast.EPre ident)
          (pexp_apply
             (pexp_ident (lident @@ string "pre"))
             (pair nolabel (pexp_ident (lident __)) ^:: nil) )
    ||| map
          ~f:(fun _ f args ->
            Lampadario.Ast.EApply (rec_parse f, List.map rec_parse args) )
          (pexp_apply __ (many (pair nolabel __)))
    ||| map
          ~f:(fun _ f args ->
            Lampadario.Ast.EApplyNoStream
              (Selected_ast.To_ocaml.copy_expression f, List.map rec_parse args)
            )
          (pattern_nostream_apply __ (many (pair nolabel __)))
    ||| map
          ~f:(fun _ expr ->
            Lampadario.Ast.ENotStream
              (Selected_ast.To_ocaml.copy_expression expr) )
          (pattern_nostream __))

let pattern_lampadario_node =
  Ast_pattern.(
    map
      ~f:(fun _ name equations return ->
        let equations =
          equations
          |> List.map (fun (ident, expr) ->
                 ( ident
                 , parse (pattern_lampadario_expr ()) expr.pexp_loc expr () ) )
          |> String.Map.of_bindings
        in
        (* todo args *)
        (name, Lampadario.Ast.{args= []; return; equations}) )
      (pstr
         ( pstr_value nonrecursive
             ( value_binding ~pat:(ppat_var __)
                 ~expr:
                   (pexp_let nonrecursive
                      (many
                         (map
                            ~f:(fun _ ident expr -> (ident, expr))
                            (value_binding ~pat:(ppat_var __) ~expr:__) ) )
                      (pexp_ident (lident __)) )
             ^:: nil )
         ^:: nil ) ))

let expand ~loc ~path:_ si =
  let name, node = (Ast_pattern.parse pattern_lampadario_node loc si) () in
  let code =
    node |> Lampadario.Compile.compile_node |> Kroonluchter.Compile.compile_node
    |> Candelabru.Compile.compile_node
    |> Mocaml.(Printer.expr_to_string)
  in
  print_endline code ;
  let buffer = Lexing.from_string code in
  Lexing.set_position buffer loc.loc_start ;
  let expr =
    Ocaml_common.Parser.parse_expression Lexer.token buffer
    |> Selected_ast.Of_ocaml.copy_expression
  in
  Ast_builder.Default.(
    pstr_value ~loc Nonrecursive
      [value_binding ~loc ~pat:(ppat_var ~loc {txt= name; loc}) ~expr])

let ext =
  Extension.declare name Extension.Context.structure_item
    Ast_pattern.(__)
    expand

let () = Ppxlib.Driver.register_transformation name ~extensions:[ext]
