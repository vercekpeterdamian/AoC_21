open Solvers.Signature
open Utils.List_utils

(* open Utils.Option_utils *)

module Solver : Solver = struct
  let read_data data = data |> List.lines |> Array.of_list

  let make_stepper (step_down, step_right) data =
    let max_row = Array.length data in
    let row_len = String.length data.(0) in
    let cycle_col x = x mod row_len in
    let rec stepper row col sum =
      if row >= max_row then sum
      else
        let su = sum + match data.(row).[col] with '#' -> 1 | _ -> 0 in
        stepper (row + step_down) (cycle_col (col + step_right)) su
    in
    stepper

  let naloga1 data = make_stepper (1, 3) (read_data data) 0 0 0 |> string_of_int

  let naloga2 data _part1 =
    let data = read_data data in
    let run f = f 0 0 0 in
    let steppers =
      List.map
        (fun x -> make_stepper x data)
        [ (1, 1); (1, 3); (1, 5); (1, 7); (2, 1) ]
    in
    let results = List.map run steppers in
    List.fold_right ( * ) results 1 |> string_of_int
end

(*
MATCH (p:Payee) -->(r:Recipient)
WITH p,r, Count(r) AS cR, p as pp, r as rr
WITH COUNT(p) as numberOfDifferentRecipients, pp
WHERE numberOfDifferentRecipients = 7
MATCH (pp)-->(r:Recipient)
RETURN pp,r, numberOfDifferentRecipients


*)
