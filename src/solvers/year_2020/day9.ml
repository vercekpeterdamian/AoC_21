open Solvers.Signature
open Utils.List_utils
open Utils.Option_utils
module IntMap = Map.Make (Int)

type state = { sums : int IntMap.t; items : int Queue.t }

let add_num { sums; items } n =
  let sums =
    Queue.fold
      (fun sums' i ->
        IntMap.update (i + n)
          (function None -> Some 1 | Some a -> Some (a + 1))
          sums')
      sums items
  in
  Queue.add n items;
  { sums; items }

let remove_num { sums; items } =
  let n = Queue.pop items in
  let sums =
    Queue.fold
      (fun sums' i ->
        IntMap.update (i + n)
          (function
            | None -> failwith "Removing wrong binding" | Some a -> Some (a - 1))
          sums')
      sums items
  in
  { sums; items }

let init_state n nums =
  let[@warning "-8"] x :: take, rest = List.split_on_n nums n in
  let state =
    List.fold_left add_num
      { sums = IntMap.empty; items = Queue.of_seq (List.to_seq [ x ]) }
      take
  in
  (state, rest)

let rec find state = function
  | [] -> None
  | x :: xs -> (
      match IntMap.find_opt x state.sums with
      | None | Some 0 -> Some x
      | Some _ ->
          let state = remove_num state in
          find (add_num state x) xs)

module Solver : Solver = struct
  let naloga1 data =
    let numbers = data |> List.lines |> List.map int_of_string in
    let state, rest = init_state 25 numbers in
    find state rest |> Option.with_default (-1) |> string_of_int

  let find_list target numbers =
    let q = Queue.create () in
    let rec find' sum = function
      | [] -> failwith "Not found"
      | x :: xs as l ->
          if sum + x = target then ()
          else if sum + x < target then (
            Queue.push x q;
            find' (sum + x) xs)
          else
            let last = Queue.pop q in
            find' (sum - last) l
    in
    find' 0 numbers;
    Queue.fold min 999999999 q + Queue.fold max 0 q

  let naloga2 data part1 =
    let part1 = int_of_string part1 in
    data |> List.lines |> List.map int_of_string |> find_list part1
    |> string_of_int
end
