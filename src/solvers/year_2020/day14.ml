open Solvers.Signature
open Utils.List_utils
open Utils.General_utils
module IntMap = Map.Make (Int)

type mask = {
  ones_mask : int;
  zeros_mask : int;
  memory : int IntMap.t;
  mask : int;
}

type update = { address : int; value : int }

type command = Mask of mask | Update of update

let parse_mask m =
  let mask = Scanf.sscanf m "mask = %s" (fun s -> s) |> List.list_of_string in
  {
    ones_mask = (List.map (( = ) '1')) mask |> bin_to_decimal;
    zeros_mask = (List.map (( = ) '0')) mask |> bin_to_decimal |> Int.lognot;
    memory = IntMap.empty;
    mask = (List.map (( = ) 'X')) mask |> bin_to_decimal;
  }

let parse_command l =
  try
    Scanf.sscanf l "mem[%d] = %s" (fun address n ->
        Update { address; value = int_of_string n })
  with _ -> Mask (parse_mask l)

let max_len = Int.shift_left 1 36

let rec generate_floating bit_mask state acc =
  if bit_mask = max_len then acc
  else
    let acc =
      if Int.logand bit_mask state.mask > 0 then
        List.map (Int.logand (Int.logand state.mask bit_mask |> Int.lognot)) acc
        @ List.map (Int.logor (Int.logand state.mask bit_mask)) acc
      else acc
    in
    generate_floating (2 * bit_mask) state acc

let rec process is_floating state = function
  | [] -> state
  | Update { address; value } :: xs ->
      let memory =
        if is_floating then
          let floating =
            generate_floating 1 state [ Int.logor address state.ones_mask ]
          in
          List.fold_left
            (fun mem f ->
              let addr' = Int.logor state.ones_mask (Int.logxor f state.mask) in
              IntMap.add addr' value mem)
            state.memory floating
        else
          IntMap.add address
            (Int.logor state.ones_mask value |> Int.logand state.zeros_mask)
            state.memory
      in
      let state = { state with memory } in
      process is_floating state xs
  | Mask mask :: xs ->
      process is_floating { mask with memory = state.memory } xs

let parse_data s =
  let[@warning "-8"] (mask :: dep) = List.lines s in
  let mask = parse_mask mask in
  (mask, List.map parse_command dep)

module Solver : Solver = struct
  let naloga1 data =
    let state, cmds = parse_data data in
    let state = process false state cmds in
    IntMap.fold (fun _ -> ( + )) state.memory 0 |> string_of_int

  let naloga2 data _part1 =
    let state, cmds = parse_data data in
    let state = process true state cmds in
    IntMap.fold (fun _ -> ( + )) state.memory 0 |> string_of_int
end
