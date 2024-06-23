
type identifier = {
  ident: string;
}
(*
type precedence = 
  | LOWEST
  | EQUALS
  | LESSGREATER
  | SUM
  | PRODUCT
  | PREFIX
  | CALL;;

type expression =  
  | Integer of int
  | Prefix of {
      operator: string;
      right: expression;
    }
  | Infix of {
      left: expression;
      operator: string;
      right: expression;
    }
  | Boolean of bool
  | If of {
      condition: expression;
      consequence: expression;
      alternative: expression;
    }
  | Function of {
      parameters: identifier list;
      body: expression;
    }
  | Call of {
      function_name: expression;
      arguments: expression list;
    };;
*)

type expression =  
  | Integer of int
  | Identifier of identifier
;;
(* Expression constructor functions so *)
let int_expr i = Integer i;;
let ident_expr i = Identifier i;;

type let_statement = {
  name: identifier;
  value: expression;
}

type statement =
  | Let of let_statement;;
(*
type return_statement = {
  value: expression;
}
type statement =
  | Let of let_statement
  | Return of return_statement;;
*)
type _program = statement list

let expression_to_string = function
  | Integer i -> Printf.sprintf "IntLiteral: %d" i
  | Identifier i -> Printf.sprintf "Identifier: %s" i.ident

let program_to_string program = 
  List.map (fun statement -> 
    match statement with
    | Let {name; value} -> 
      Printf.sprintf "let %s = %s;\n" name.ident (expression_to_string value) 
  ) program

let parser = let open Angstrom in 
  (* Helper parsers *)
  let whitespace = take_while (function ' ' -> true | '\n' -> true | '\t' -> true | _ -> false) in

  (* Expression Parsers*)
  let integer_literal = take_while1 (function '0' .. '9' -> true | _ -> false) >>| (fun i -> i |> int_of_string |> int_expr) in  
  let identifier_parser = 
    take_while1  (function 'a' .. 'z' -> true | 'A' .. 'Z' -> true | '0'..'9' -> true | _ -> false) >>= fun name ->
    return {ident = name} in
  let identifier_parser_expr = 
    take_while1  (function 'a' .. 'z' -> true | 'A' .. 'Z' -> true | '0'..'9' -> true | _ -> false) >>= fun name ->
    return (ident_expr {ident = name}) in
  let expression_parser = 
    choice [ integer_literal; identifier_parser_expr ] in
  
  (* Statement parsers *)
  let let_statement_parser =
    whitespace *>
    string "let" *> 
    whitespace *>
    identifier_parser >>= fun ident ->
    whitespace *>
    char '=' *>
    whitespace *>
    expression_parser >>= fun expr ->
    whitespace *>
    char ';' *>
    whitespace *>
    return (Let {name=ident; value=expr}) in
  many @@ choice [let_statement_parser] 

let test_input = "
let five = 5;
let ten = 10;
"
let _test_input_2 = "
let five = 5;

let ten = 10;

let add = fn(x, y) {
  x + y;
};

let result = add(ten, five);"
let _test_input_1 = "let five = 5;
let ten = 10;

let add = fn(x, y) {
  x + y;
};"
let () = let open Angstrom in 
  match parse_string ~consume:All parser test_input with
  | Ok program -> program |> program_to_string |> List.iter print_endline  
  | Error msg -> print_endline msg

