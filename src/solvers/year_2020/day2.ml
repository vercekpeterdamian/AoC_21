open Solvers.Signature
open Utils.List_utils

(* open Utils.Option_utils *)

module Solver : Solver = struct
  (* Disable bad pattern matching warning *)
  let[@warning "-8"] split_line l =
    let [ pol; pass ] = String.split_on_char ':' l in
    let [ dur; ch ] = String.split_on_char ' ' pol in
    let [ mi; ma ] = String.split_on_char '-' dur in
    ( (int_of_string mi, int_of_string ma),
      ch.[0],
      String.sub pass 1 (String.length pass - 1) )

  let is_valid ((mi, ma), ch, pass) =
    let count = List.count ch (List.list_of_string pass) in
    mi <= count && count <= ma

  let naloga1 data =
    let lines = data |> List.lines |> List.map split_line in
    lines |> List.count_filter is_valid |> string_of_int

  let is_valid2 ((mi, ma), ch, pass) =
    let mi = mi - 1 in
    let ma = ma - 1 in
    let same = pass.[mi] = pass.[ma] in
    let correct_chars = pass.[mi] = ch || pass.[ma] = ch in
    let _ = (not same) && correct_chars in
    (* This is actually xor *)
    pass.[mi] = ch != (pass.[ma] = ch)

  let naloga2 data _part1 =
    let lines = data |> List.lines |> List.map split_line in
    List.count_filter2 is_valid2 lines |> string_of_int
end
