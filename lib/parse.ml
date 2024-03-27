open Ast
open Tokens

let rec primary tokens =
  match tokens with
  | { kind = Tokens.True; _ } :: rest -> (Literal(Bool(true)), rest)
  | { kind = Tokens.False; _ } :: rest -> (Literal(Bool(false)), rest)
  | _ :: rest -> (Error("Unhandled token"), rest)
  | [] -> Literal(Nil), []

and unary tokens =
  match tokens with
  | ({ kind = Tokens.Bang; _ } | { kind = Tokens.Minus; _ }) as op :: rest ->
     let right, rrest = primary rest in
     (Unary(op, right), rrest)
  | _ -> primary tokens

and term tokens =
  let left, rest = unary tokens in
  match rest with
  | ({ kind = Tokens.Plus; _ } | { kind = Tokens.Minus; _ }) as op :: rest ->
     let right, rrest = term rest in
     (Binary(left, op, right), rrest)
  | _ -> (left, rest)

and factor tokens =
  let left, rest = term tokens in
  match rest with
  | ({ kind = Tokens.Star; _ } | { kind = Tokens.Slash; _ }) as op :: rest ->
     let right, rrest = term rest in
     (Binary(left, op, right), rrest)
  | _ -> (left, rest)

and comparision tokens =
  let left, rest = factor tokens in
  match rest with
  | ({ kind = Tokens.Greater; _ } | { kind = Tokens.GreaterEqual; _ }
    | { kind = Tokens.Less; _ }  | { kind = Tokens.LessEqual; _ } | { kind = Tokens.Equal; _ }) as op :: rest ->
     let right, rrest = factor rest in
     (Binary(left, op, right), rrest)
  | _ -> (left, rest)

and equality tokens =
  let left, rest = comparision tokens in
  match rest with
  | ({ kind = Tokens.BangEqual; _ } | { kind = Tokens.EqualEqual; _ }) as op :: rest ->
     let right, rrest = comparision rest in
     (Binary(left, op, right), rrest)
  | _ -> (left, rest)

and expression tokens = equality tokens

let extract_first f _ = f

let parse tokens =
  let ast, _ = expression tokens in
  ast
