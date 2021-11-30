open Solvers.Signature
open Utils.List_utils
open Utils.String_utils

module Solver : Solver = struct
  open Opal

  type expression =
    | PlusE of expression * expression
    | TimesE of expression * expression
    | Const of int

  let number = spaces >> many1 digit => implode % int_of_string

  let parens = between (token "(") (token ")")

  let addop = token "+" >> return (fun x y -> PlusE (x, y))

  let mulop = token "*" >> return (fun x y -> TimesE (x, y))

  let atom = number => fun x -> Const x

  let rec expr input = (chainl1 prm_expr (mulop <|> addop)) input

  and prm_expr input = (parens expr <|> atom) input

  let rec expr2 input = (chainl1 mul_expr mulop) input

  and mul_expr input = (chainl1 add_expr mulop) input

  and add_expr input = (chainl1 prm_expr addop) input

  and prm_expr input = (parens expr2 <|> atom) input

  let rec eval = function
    | Const n -> n
    | PlusE (a, b) -> eval a + eval b
    | TimesE (a, b) -> eval a * eval b

  let naloga1 data =
    let p_eval s =
      let[@warning "-8"] (Some ans) = LazyStream.of_string s |> parse expr in
      eval ans
    in
    List.sum (List.map p_eval (String.split_on_char '\n' data)) |> string_of_int

  let naloga2 data _part1 =
    let p_eval s =
      let[@warning "-8"] (Some ans) = LazyStream.of_string s |> parse expr2 in
      eval ans
    in
    List.sum (List.map p_eval (String.split_on_char '\n' data)) |> string_of_int
end
