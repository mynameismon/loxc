open Error

type result =
  | Float of float
  | Bool of bool
  | String of string
  | Error of error
  | NilVal

let rec eval ast =
  match ast with
  | Ast.Literal Ast.Nil -> NilVal
  | Ast.Literal (Number num) -> Float num
  | Ast.Literal (Bool b) -> Bool b
  | Ast.Grouping g -> eval g
  | Ast.Literal (String str) -> String str
  | Ast.Binary (left, op, right) -> (
      let l = eval left and r = eval right in
      match (l, op, r) with
      | Float a, Plus, Float b -> Float (a +. b)
      | Float a, Minus, Float b -> Float (a -. b)
      | Float a, Star, Float b -> Float (a *. b)
      | Float a, Slash, Float b -> Float (a /. b)
      | Float a, Greater, Float b -> Bool (a > b)
      | Float a, GreaterEqual, Float b -> Bool (a >= b)
      | Float a, Less, Float b -> Bool (a < b)
      | Float a, LessEqual, Float b -> Bool (a <= b)
      | Float a, EqualEqual, Float b -> Bool (a = b)
      | Float a, BangEqual, Float b -> Bool (a <> b)
      | String a, Plus, String b -> String (a ^ b)
      | Bool a, Greater, Bool b -> Bool (a > b)
      | Bool a, GreaterEqual, Bool b -> Bool (a >= b)
      | Bool a, Less, Bool b -> Bool (a < b)
      | Bool a, LessEqual, Bool b -> Bool (a <= b)
      | Bool a, EqualEqual, Bool b -> Bool (a = b)
      | Bool a, BangEqual, Bool b -> Bool (a <> b)
      | Error a, _, _ -> Error a
      | _, _, Error a -> Error a
      | _ -> Error (RunTimeError "Unhandled Binary Operator"))
  | Ast.Unary (op, right) -> (
      let r = eval right in
      match (op, r) with
      | Minus, Float a -> Float (0. -. a)
      | Bang, Bool b -> Bool (not b)
      | Error a, _ -> Error a
      | _, Error b -> Error b
      | _ -> Error (InternalError "Should not be reached"))
  | Ast.Error err -> Error err
  | _ -> Error (RunTimeError "Unhandled Token")

let print_result res =
  match res with
  | Float f -> Printf.sprintf "%f" f
  | Bool b -> ( match b with true -> "true" | false -> "false")
  | String str -> str
  | Error e -> print_error e
  | NilVal -> "Nil"

let interpret ast = eval ast |> print_result
