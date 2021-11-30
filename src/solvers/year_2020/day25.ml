open Solvers.Signature
open Utils.List_utils

let rec pow a n m =
  if n = 0 then 1
  else if n = 1 then a mod m
  else
    let b = pow a (n / 2) m in
    b * b mod m * (if n mod 2 = 0 then 1 else a) mod m

type state = { card : int; door : int }

let parse d =
  let[@warning "-8"] [ card; door ] =
    d |> List.lines |> List.map int_of_string
  in
  { card; door }

let memo = Hashtbl.create 100_000

let subject_g = 7

(* Is prime, group is cyclic *)
let mod_p = 2020_1227

exception Done of int

let find factor m h =
  let e = ref h in
  try
    for i = 0 to m - 1 do
      match Hashtbl.find_opt memo !e with
      | None -> e := !e * factor mod mod_p
      | Some x -> raise (Done ((i * m) + x))
    done;
    failwith "Not found"
  with Done x -> x

let baby_step_giant_step state =
  let m = ceil (sqrt (float mod_p)) |> int_of_float in
  let prev = ref 1 in
  for j = 0 to m - 1 do
    Hashtbl.add memo !prev j;
    prev := !prev * subject_g mod mod_p
  done;
  let factor = pow subject_g (mod_p - m - 1) mod_p in
  (find factor m state.door, find factor m state.card)

module Solver : Solver = struct
  let naloga1 data =
    let state = parse data in
    let d, _ = baby_step_giant_step state in
    pow state.card d mod_p |> string_of_int

  let naloga2 _data _part1 = "It is a day of joy :)"
end
