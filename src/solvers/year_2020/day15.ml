open Solvers.Signature
open Utils.List_utils
module IntMap = Map.Make (Int)

module Solver : Solver = struct
  type state = { nums : int list IntMap.t; spoken : int }

  let add num turn =
    IntMap.update num (function
      | None -> Some [ turn ]
      | Some x -> (
          match x with
          | [] -> Some [ turn ]
          | [ a ] | a :: _ -> Some [ turn; a ]))

  let parse_data s =
    let nums = List.map int_of_string (s |> String.split_on_char ',') in
    ( {
        nums =
          List.fold_left
            (fun (i, m) n -> (i + 1, add n i m))
            (0, IntMap.empty) nums
          |> snd;
        spoken = List.hd (List.rev nums);
      },
      List.length nums )

  let rec step turn until ({ spoken; nums } as state) =
    if until <= turn then state
    else
      let last = spoken in
      let say =
        match IntMap.find_opt last nums with
        | None | Some [] | Some [ _ ] -> 0
        | Some (l :: prev :: _) -> l - prev
      in
      step (turn + 1) until { spoken = say; nums = add say turn nums }

  let naloga1 data =
    let state, l = parse_data data in
    let state' = step l 2020 state in
    state'.spoken |> string_of_int

  (* Works under 2 minutes, is fast to write and my head hurts, good enough :) *)
  let naloga2 data _part1 =
    let state, l = parse_data data in
    let state' = step l 30000000 state in
    state'.spoken |> string_of_int
end
