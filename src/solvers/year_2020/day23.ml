open Solvers.Signature
open Utils.List_utils
open Utils.String_utils
open Utils.General_utils
open Utils.Option_utils
module IntMap = Map.Make (Int)

type 'a node = { mutable data : 'a; mutable next : 'a node option }

let parse l =
  l |> List.list_of_string |> List.map Char.escaped |> List.map int_of_string

let wrap i = if i = 0 then 9 else i

let split_at i l =
  let rec aux acc = function
    | x :: xs when i = x -> (List.rev (x :: acc), xs)
    | x :: xs -> aux (x :: acc) xs
    | [] -> failwith "Invalid list"
  in
  aux [] l

let print_list l = l |> List.map string_of_int |> String.concat ","

let step l =
  let f = List.hd l in
  let t = List.tl l in
  let pick, rest = List.take3 t in
  let rec find_dest i =
    let i = wrap i in
    if List.mem i pick then find_dest (i - 1)
    else (* Printf.printf "dest: %d\n" i; *)
      split_at i rest
  in
  (*
  let s = print_list pick in
  Printf.printf "f: %d; dest1: %d; mem: %s\n" f (f - 1) s;*)
  let bef, aft = find_dest (wrap (f - 1)) in
  (bef @ pick) @ aft @ [ f ]

let max_len = 1_000_000

let wrap_ll i = if i = 0 then max_len else i

let parse2 l =
  let l = parse l in
  let m = List.maximum l in
  l @ List.init (max_len - List.length l) (fun x -> m + x + 1)

type 'a state = { head : 'a node; tail : 'a node; location : 'a node IntMap.t }

let build_state l =
  let head = { data = List.hd l; next = None } in
  let map = IntMap.singleton (List.hd l) head in
  let folder (prev, map) x =
    let node = { data = x; next = None } in
    let map = IntMap.add x node map in
    prev.next <- Some node;
    (node, map)
  in
  let tail, map = List.fold_left folder (head, map) (List.tl l) in
  tail.next <- Some head;
  { head; tail; location = map }

let prepare_state = 1

let step_ll state node =
  let n1 = Option.force_val node.next in
  let n2 = Option.force_val n1.next in
  let n3 = Option.force_val n2.next in
  let up_vals = [ n1.data; n2.data; n3.data ] in
  let rec find_val i =
    let i = wrap_ll i in
    if List.mem i up_vals then find_val (i - 1)
    else (* Printf.printf "dest: %d\n" i; *)
      i
  in

  let target_node = IntMap.find (find_val (node.data - 1)) state.location in
  (* Rewire things *)
  let target_next = target_node.next |> Option.force_val in
  target_node.next <- Some n1;
  node.next <- n3.next;
  n3.next <- Some target_next;
  Option.force_val node.next

module Solver : Solver = struct
  let naloga1 data =
    let data = parse data in
    let final = apply step data 100 in
    let f, s = final |> split_at 1 in
    List.take 8 (s @ f) |> List.map string_of_int |> String.concat ""

  let naloga2 data _part1 =
    let data = parse2 data in
    let state = build_state data in
    let _ = apply (step_ll state) state.head 10_000_000 in
    let res = IntMap.find 1 state.location in
    let f = Option.force_val res.next in
    let s = Option.force_val f.next in
    s.data * f.data |> string_of_int
end
