open Solvers.Signature
open Utils.List_utils
open Utils.Option_utils

module Solver : Solver = struct
  let rec find_s target l1 l2 =
    match (l1, l2) with
    | x :: _, y :: _ when x + y = target -> Some (x * y)
    | x :: xs, y :: _ when x + y < target -> find_s target xs l2
    | _ :: _, _ :: ys -> find_s target l1 ys
    | _ -> None

  (* O (n log n), but could be better with hashmap -> O(n)*)
  let naloga1 data =
    let lines = data |> List.lines |> List.int_list |> List.sort compare in
    let rev = List.rev lines in
    string_of_int (Option.with_default (-1) (find_s 2020 lines rev))

  let rec process_2 full rev = function
    | [] -> failwith "Invalid assumptions"
    | x :: xs -> (
        match find_s (2020 - x) full rev with
        | Some d -> x * d
        | None -> process_2 full rev xs)

  (* O(n^2), I think it can be better *)
  let naloga2 data _part1 =
    let lines = data |> List.lines |> List.int_list |> List.sort compare in
    let rev = List.rev lines in
    process_2 lines rev lines |> string_of_int
end
