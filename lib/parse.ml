open Ast
open Tokens

let rec primary tokens =
  match tokens with
  | { kind = Tokens.True; _ } :: rest -> (Literal (Bool true), rest)
  | { kind = Tokens.False; _ } :: rest -> (Literal (Bool false), rest)
  | { kind = Tokens.Number num; _ } :: rest -> (Literal (Number num), rest)
  | { kind = Tokens.Nil; _ } :: rest -> (Literal Nil, rest)
  | { kind = Tokens.String str; _ } :: rest -> (Literal (String str), rest)
  | { kind = Tokens.LeftParen; _ } :: rest -> (
      let e, rrest = expression rest in
      match rrest with
      | { kind = Tokens.RightParen; _ } :: rrest -> (Grouping e, rrest)
      | { kind = _ as tok; _ } :: rest ->
          ( Error
              (SynError
                 (Printf.sprintf "Paren - unhandled token %s"
                    (token_to_string tok))),
            rest )
      | [] -> (Error (SynError "Unclosed bracket!"), []))
  | { kind = Tokens.RightParen; _ } :: rest -> (Literal Nil, rest)
  | { kind = _ as tok; _ } :: rest ->
      ( Error
          (SynError (Printf.sprintf "Unhandled Token %s" (token_to_string tok))),
        rest )
  | [] -> (Literal Nil, [])

and unary tokens =
  match tokens with
  | (({ kind = Tokens.Bang; _ } | { kind = Tokens.Minus; _ }) as op) :: rest ->
      let right, rrest = primary rest in
      (Unary (op.kind, right), rrest)
  | _ -> primary tokens

and term tokens =
  let left, rest = unary tokens in
  match rest with
  | (({ kind = Tokens.Plus; _ } | { kind = Tokens.Minus; _ }) as op) :: rest ->
      let right, rrest = term rest in
      (Binary (left, op.kind, right), rrest)
  | _ -> (left, rest)

and factor tokens =
  let left, rest = term tokens in
  match rest with
  | (({ kind = Tokens.Star; _ } | { kind = Tokens.Slash; _ }) as op) :: rest ->
      let right, rrest = factor rest in
      (Binary (left, op.kind, right), rrest)
  | _ -> (left, rest)

and comparision tokens =
  let left, rest = factor tokens in
  match rest with
  | (( { kind = Tokens.Greater; _ }
     | { kind = Tokens.GreaterEqual; _ }
     | { kind = Tokens.Less; _ }
     | { kind = Tokens.LessEqual; _ }
     | { kind = Tokens.Equal; _ } ) as op)
    :: rest ->
      let right, rrest = comparision rest in
      (Binary (left, op.kind, right), rrest)
  | _ -> (left, rest)

and equality tokens =
  let left, rest = comparision tokens in
  match rest with
  | (({ kind = Tokens.BangEqual; _ } | { kind = Tokens.EqualEqual; _ }) as op)
    :: rest ->
      let right, rrest = equality rest in
      (Binary (left, op.kind, right), rrest)
  | _ -> (left, rest)

and expression tokens = equality tokens

and expr_stmt tokens ast =
  let expr, rest = expression tokens in
  match rest with
  | { kind = Tokens.Semicolon; _ } :: rrest -> (rrest, Ast.Expr expr :: ast)
  | _ :: rrest ->
      ( rrest,
        Ast.Error (SynError "Incomplete line. Did you forget the semicolon?")
        :: Ast.Expr expr :: ast )
  | [] -> ([], Ast.Error (SynError "Unexpected EOF!") :: Ast.Expr expr :: ast)

and statement tokens ast =
  match tokens with
  | { kind = Tokens.Print; _ } :: rest -> (
      let expr, rrest = expression rest in
      match rrest with
      | { kind = Tokens.Semicolon; _ } :: rrest' ->
          statement rrest' (Ast.Print expr :: ast)
      | _ ->
          ( [],
            Ast.Error
              (SynError "Incomplete line. Did you forget the semicolon?")
            :: Ast.Print expr :: ast ))
  | [] -> (tokens, ast)
  | _ ->
      let rest, ast = expr_stmt tokens ast in
      statement rest ast

let parse tokens =
  let _, ast = statement tokens [] in
  ast |> List.rev

(* successful tests *)
let%test "parse strings" =
  parse
    [
      { kind = String "Hello world"; line_no = 1; col = 1 };
      { kind = Semicolon; line_no = 1; col = 13 };
    ]
  = [ Expr (Literal (String "Hello world")) ]

let%test "parse false" =
  parse
    [
      { kind = False; line_no = 1; col = 1 };
      { kind = Semicolon; line_no = 1; col = 5 };
    ]
  = [ Expr (Literal (Bool false)) ]

let%test "parse true" =
  parse
    [
      { kind = True; line_no = 1; col = 1 };
      { kind = Semicolon; line_no = 1; col = 4 };
    ]
  = [ Expr (Literal (Bool true)) ]

let%test "parse negative" =
  parse
    [
      { kind = Minus; line_no = 1; col = 1 };
      { kind = Number 1.; line_no = 1; col = 2 };
      { kind = Semicolon; line_no = 1; col = 3 };
    ]
  = [ Expr (Unary (Minus, Literal (Number 1.))) ]

let%test "parse sum" =
  parse
    [
      { kind = Number 1.; line_no = 1; col = 1 };
      { kind = Plus; line_no = 1; col = 2 };
      { kind = Number 1.; line_no = 1; col = 3 };
      { kind = Semicolon; line_no = 1; col = 4 };
    ]
  = [ Expr (Binary (Literal (Number 1.), Plus, Literal (Number 1.))) ]

let%test "parse factor" =
  parse
    [
      { kind = Number 1.; line_no = 1; col = 1 };
      { kind = Star; line_no = 1; col = 2 };
      { kind = Number 1.; line_no = 1; col = 3 };
      { kind = Semicolon; line_no = 1; col = 4 };
    ]
  = [ Expr (Binary (Literal (Number 1.), Star, Literal (Number 1.))) ]

let%test "parse comparision" =
  parse
    [
      { kind = Number 1.; line_no = 1; col = 1 };
      { kind = Greater; line_no = 1; col = 2 };
      { kind = Number 1.; line_no = 1; col = 3 };
      { kind = Semicolon; line_no = 1; col = 4 };
    ]
  = [ Expr (Binary (Literal (Number 1.), Greater, Literal (Number 1.))) ]

let%test "parse equality" =
  parse
    [
      { kind = Number 1.; line_no = 1; col = 1 };
      { kind = EqualEqual; line_no = 1; col = 2 };
      { kind = Number 1.; line_no = 1; col = 4 };
      { kind = Semicolon; line_no = 1; col = 4 };
    ]
  = [ Expr (Binary (Literal (Number 1.), EqualEqual, Literal (Number 1.))) ]

(* (\* Unsuccessful tests *\) *)
let%test "invalid parse operators" =
  parse
    [
      { kind = Plus; line_no = 1; col = 1 };
      { kind = Number 1.; line_no = 1; col = 3 };
    ]
  = [
      Expr (Error (SynError "Unhandled Token +"));
      Ast.Error (SynError "Incomplete line. Did you forget the semicolon?");
    ]

let%test "invalid parse comparision" =
  parse
    [
      { kind = Plus; line_no = 1; col = 1 };
      { kind = Less; line_no = 1; col = 2 };
      { kind = Number 1.; line_no = 1; col = 3 };
    ]
  = [
      Expr
        (Binary (Error (SynError "Unhandled Token +"), Less, Literal (Number 1.)));
      Ast.Error (SynError "Unexpected EOF!");
    ]

let%test "parse numbers" =
  parse [ { kind = Number 1.; line_no = 1; col = 1 } ]
  = [ Expr (Literal (Number 1.)); Ast.Error (SynError "Unexpected EOF!") ]
