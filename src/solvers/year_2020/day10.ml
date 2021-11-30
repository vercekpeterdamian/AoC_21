open Solvers.Signature
open Utils.List_utils

module Solver : Solver = struct
  let naloga1 data =
    let numbers =
      data |> List.lines |> List.map int_of_string |> List.sort Stdlib.compare
    in
    let _, (one, three) =
      List.fold_left
        (fun (last, (one, three)) n ->
          ( n,
            match n - last with
            | 1 -> (one + 1, three)
            | 3 -> (one, three + 1)
            | _ -> failwith "Invalid" ))
        (0, (0, 1))
        numbers
    in
    one * three |> string_of_int

  let recurrence l =
    let rec recur (a0, a1, a2) n = function
      | [] -> []
      | x :: xs when x = n -> a2 :: recur (a0, a1, a2) n xs
      | l -> recur (a1, a2, a0 + a1 + a2) (n + 1) l
    in
    recur (1, 1, 2) 2 (List.filter (( != ) 1) l)

  let naloga2 data _part1 =
    let numbers =
      data |> List.lines |> List.map int_of_string |> List.sort Stdlib.compare
    in
    let numbers =
      numbers @ [ List.nth numbers (List.length numbers - 1) + 3 ]
    in
    let lens, _ =
      List.fold_left
        (fun (lens, (last, len)) n ->
          match n - last with
          | 1 -> (lens, (n, len + 1))
          | 3 when len != 0 -> (len :: lens, (n, 0))
          | 3 -> (lens, (n, 0))
          | _ -> failwith "Invalid")
        ([], (0, 0))
        numbers
    in
    let lens = List.sort Stdlib.compare lens in
    List.fold_left ( * ) 1 (recurrence lens) |> string_of_int
end
