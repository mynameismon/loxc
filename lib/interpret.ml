type result =
  | Float of float
  | Bool of bool
  | Error of string

let rec eval ast =
  match ast with
  | Ast.Literal(Number num) -> Float num
  | Ast.Literal(Bool b) -> Bool b
  | Ast.Grouping(g) -> eval g
  | Ast.Binary(left, op, right) -> (
    let l = eval left
    and r = eval right in
    match (l, op, r) with
      
    | Float(a), { kind = Tokens.Plus; _ }, Float(b) -> Float (a +. b)
    | Float(a), { kind = Tokens.Minus; _ }, Float(b) -> Float (a -. b)
    | Float(a), { kind = Tokens.Star; _ }, Float(b) -> Float (a *. b)
    | Float(a), { kind = Tokens.Slash; _ }, Float(b) -> Float (a /. b)
    | Float(a), { kind = Tokens.Greater; _ }, Float(b) -> Bool (a > b)
    | Float(a), { kind = Tokens.GreaterEqual; _ }, Float(b) -> Bool (a >= b)
    | Float(a), { kind = Tokens.Less; _ }, Float(b) -> Bool (a < b)
    | Float(a), { kind = Tokens.LessEqual; _ }, Float(b) -> Bool (a <= b)
    | Float(a), { kind = Tokens.EqualEqual; _ }, Float(b) -> Bool (a = b)
    | Float(a), { kind = Tokens.BangEqual; _ }, Float(b) -> Bool (a <> b)

    | Bool(a), { kind = Tokens.Greater; _ }, Bool(b) -> Bool (a > b)
    | Bool(a), { kind = Tokens.GreaterEqual; _ }, Bool(b) -> Bool (a >= b)
    | Bool(a), { kind = Tokens.Less; _ }, Bool(b) -> Bool (a < b)
    | Bool(a), { kind = Tokens.LessEqual; _ }, Bool(b) -> Bool (a <= b)
    | Bool(a), { kind = Tokens.EqualEqual; _ }, Bool(b) -> Bool (a = b)
    | Bool(a), { kind = Tokens.BangEqual; _ }, Bool(b) -> Bool (a <> b)

    | Error(a), _, _ -> Error(a)
    | _, _, Error(a) -> Error(a)
    | _ -> Error "Unhandled"
  )
  | Ast.Error err -> Error err
  | _ -> Error "Unhandled nodes"

let print_result res =
  match res with
  | Float f -> Printf.sprintf "%f" f
  | Bool b -> (
    match b with
    | true -> "true"
    | false -> "false"
  )
  | Error e -> e

let interpret ast = eval ast |> print_result
