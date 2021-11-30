open Solvers.Signature
open Utils.List_utils

module Solver : Solver = struct
  module CharSet = Set.Make (Char)

  let add_chars_to_set s =
    let l = List.list_of_string s in
    List.fold_left (fun m' c -> CharSet.add c m') CharSet.empty l

  let process_batch fn l = l |> List.map add_chars_to_set |> List.reduce2 fn

  let naloga1 data =
    data |> List.lines |> List.groups |> List.map String.trim
    |> List.map (String.split_on_char ' ')
    |> List.map (process_batch CharSet.union)
    |> List.map CharSet.cardinal |> List.sum |> string_of_int

  let naloga2 data _part1 =
    data |> List.lines |> List.groups |> List.map String.trim
    |> List.map (String.split_on_char ' ')
    |> List.map (process_batch CharSet.inter)
    |> List.map CharSet.cardinal |> List.sum |> string_of_int
end
