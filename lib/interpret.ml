open Error

type result =
  | Float of float
  | Bool of bool
  | Error of error

let rec eval ast =
  match ast with
  | Ast.Literal(Number num) -> Float num
  | Ast.Literal(Bool b) -> Bool b
  | Ast.Grouping(g) -> eval g
  | Ast.Binary(left, op, right) -> (
    let l = eval left
    and r = eval right in
    match (l, op, r) with
      
    | Float(a), Plus, Float(b) -> Float (a +. b)
    | Float(a), Minus, Float(b) -> Float (a -. b)
    | Float(a), Star, Float(b) -> Float (a *. b)
    | Float(a), Slash, Float(b) -> Float (a /. b)
    | Float(a), Greater, Float(b) -> Bool (a > b)
    | Float(a), GreaterEqual, Float(b) -> Bool (a >= b)
    | Float(a), Less, Float(b) -> Bool (a < b)
    | Float(a), LessEqual, Float(b) -> Bool (a <= b)
    | Float(a), EqualEqual, Float(b) -> Bool (a = b)
    | Float(a), BangEqual, Float(b) -> Bool (a <> b)

    | Bool(a), Greater, Bool(b) -> Bool (a > b)
    | Bool(a), GreaterEqual, Bool(b) -> Bool (a >= b)
    | Bool(a), Less, Bool(b) -> Bool (a < b)
    | Bool(a), LessEqual, Bool(b) -> Bool (a <= b)
    | Bool(a), EqualEqual, Bool(b) -> Bool (a = b)
    | Bool(a), BangEqual, Bool(b) -> Bool (a <> b)

    | Error(a), _, _ -> Error(a)
    | _, _, Error(a) -> Error(a)
    | _ -> Error (RunTimeError ("Unhandled Binary Operator"))
  )
  | Ast.Error err -> Error err
  | _ -> Error (RunTimeError ("Unhandled Token"))

let print_result res =
  match res with
  | Float f -> Printf.sprintf "%f" f
  | Bool b -> (
    match b with
    | true -> "true"
    | false -> "false"
  )
  | Error e -> print_error e

let interpret ast = eval ast |> print_result
