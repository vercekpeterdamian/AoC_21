open Solvers.Signature
open Utils.List_utils

module Solver : Solver = struct
  type bus = { id : int; index : int; waiting : int }

  type timetable = { arrival_time : int; departures : bus option list }

  (*
  Chineese remainer theorem solving
  Taken from: https://rosettacode.org/wiki/Chinese_remainder_theorem#OCaml
*)
  let rec egcd a b =
    if b = 0 then (1, 0)
    else
      let q = a / b and r = a mod b in
      let s, t = egcd b r in
      (t, s - (q * t))

  let mod_inv a b =
    let x, y = egcd a b in
    if (a * x) + (b * y) = 1 then Some x else None

  let calc_inverses ns ms =
    let rec list_inverses ns ms l =
      match (ns, ms) with
      | [], [] -> Some l
      | [], _ | _, [] -> assert false
      | n :: ns, m :: ms -> (
          let inv = mod_inv n m in
          match inv with None -> None | Some v -> list_inverses ns ms (v :: l))
    in
    Option.map (fun l -> List.rev l) (list_inverses ns ms [])

  (*

*)

  let parse_timetable s =
    let[@warning "-8"] [ arr; dep ] = List.lines s in
    let deps =
      List.mapi (fun i x ->
          match x with
          | "x" -> None
          | x -> Some { index = i; id = int_of_string x; waiting = -1 })
      @@ String.split_on_char ',' dep
    in
    { arrival_time = int_of_string arr; departures = deps }

  let waiting_time arrival_time bus =
    let w = bus.id - (arrival_time mod bus.id) in
    { bus with waiting = w }

  let naloga1 data =
    let timetable = parse_timetable data in
    let waiting =
      List.map
        (waiting_time timetable.arrival_time)
        (List.filter_map (fun x -> x) timetable.departures)
    in
    let best =
      List.fold_left
        (fun s b -> if b.waiting < s.waiting then b else s)
        (List.nth waiting 0) waiting
    in
    best.id * best.waiting |> string_of_int

  let naloga2 data _part1 =
    let timetable = parse_timetable data in
    let clean = List.filter_map (fun x -> x) timetable.departures in
    let residues = List.map (fun b -> b.index) clean in
    let modulii = List.map (fun b -> b.id) clean in
    let mod_pi = List.reduce ( * ) modulii in
    let crt_modulii = List.map (fun m -> mod_pi / m) modulii in
    let[@warning "-8"] (Some inverses) = calc_inverses crt_modulii modulii in
    let x =
      List.map3 (fun a b c -> a * b * c) residues inverses crt_modulii
      |> List.reduce ( + )
      |> fun n ->
      let n' = n mod mod_pi in
      if n' < 0 then n' + mod_pi else n'
    in
    mod_pi - (x mod mod_pi) |> string_of_int
end
