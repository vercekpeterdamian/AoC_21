open Solvers.Signature
open Utils.List_utils

module Solver : Solver = struct
  type seat = {
    seat : string;
    row_code : string;
    col_code : string;
    row : int;
    col : int;
    id : int;
  }

  let bin_to_decimal l =
    let rec bin_to_s a = function
      | [] -> a
      | x :: xs -> if x then bin_to_s ((2 * a) + 1) xs else bin_to_s (2 * a) xs
    in
    bin_to_s 0 l

  let flt t l = List.map (fun x -> x = t) l

  let parse_seat s =
    let row = String.sub s 0 7 in
    let col = String.sub s 7 3 in
    let row_b = flt 'B' (row |> List.list_of_string) in
    let col_b = flt 'R' (col |> List.list_of_string) in
    let row_n = bin_to_decimal row_b in
    let col_n = bin_to_decimal col_b in
    {
      seat = s;
      row_code = row;
      col_code = col;
      row = row_n;
      col = col_n;
      id = (row_n * 8) + col_n;
    }

  let naloga1 data =
    data |> List.lines |> List.map parse_seat
    |> List.map (fun x -> x.id)
    |> List.maximum |> string_of_int

  let rec find_missing i = function
    | [] -> failwith "No missing"
    | s :: xs when i = s -> find_missing (i + 1) xs
    | _ -> i

  let naloga2 data _part1 =
    let seats =
      data |> List.lines |> List.map parse_seat
      |> List.sort (fun x y -> x.id - y.id)
    in
    let ids = List.map (fun x -> x.id) seats in
    find_missing (List.nth ids 0) ids |> string_of_int
end
