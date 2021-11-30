open Solvers.Signature
open Utils.List_utils
open Utils.General_utils

type coord = int * int

module CoordMap = Map.Make (struct
  type t = coord

  let compare = Stdlib.compare
end)

module CoordSet = Set.Make (struct
  type t = coord

  let compare = Stdlib.compare
end)

let ( ++ ) ((a, b) : coord) ((c, d) : coord) : coord = (a + c, b + d)

type direction = E | Se | Sw | W | Nw | Ne

let all_directions = [ E; Se; Sw; W; Nw; Ne ]

let dir_to_vec : direction -> coord = function
  | E -> (1, 0)
  | Se -> (0, 1)
  | Sw -> (-1, 1)
  | W -> (-1, 0)
  | Nw -> (0, -1)
  | Ne -> (1, -1)

let neighbours c = List.map (( ++ ) c) (List.map dir_to_vec all_directions)

let parse_line l =
  let rec aux = function
    | 'e' :: rest -> E :: aux rest
    | 'w' :: rest -> W :: aux rest
    | 's' :: 'e' :: rest -> Se :: aux rest
    | 's' :: 'w' :: rest -> Sw :: aux rest
    | 'n' :: 'e' :: rest -> Ne :: aux rest
    | 'n' :: 'w' :: rest -> Nw :: aux rest
    | [] -> []
    | _ -> failwith "Invalid input"
  in

  l |> List.list_of_string |> aux

let move loc l = List.fold_left ( ++ ) loc (List.map dir_to_vec l)

let parse d = d |> List.lines |> List.map parse_line

let step map =
  let interesting =
    CoordSet.fold
      (fun coord acc ->
        CoordSet.union (CoordSet.of_list (coord :: neighbours coord)) acc)
      map CoordSet.empty
  in
  let out =
    CoordSet.fold
      (fun k acc ->
        let res =
          List.filter (fun x -> CoordSet.mem x map) (neighbours k)
          |> List.length
        in
        let res =
          if CoordSet.mem k map then (* black *)
            not (res = 0 || res > 2)
          else res = 2
        in
        if res then k :: acc else acc)
      interesting []
  in
  out |> CoordSet.of_list

module Solver : Solver = struct
  let naloga1 data =
    let data = parse data in
    let map =
      List.fold_left
        (fun map moves ->
          let final = move (0, 0) moves in
          CoordMap.update final
            (function Some x -> Some (x + 1) | None -> Some 1)
            map)
        CoordMap.empty data
    in
    map |> CoordMap.bindings |> List.map snd
    |> List.count_filter (fun x -> x mod 2 = 1)
    |> string_of_int

  let naloga2 data _part1 =
    let data = parse data in
    let map =
      List.fold_left
        (fun map moves ->
          let final = move (0, 0) moves in
          CoordMap.update final
            (function Some x -> Some (x + 1) | None -> Some 1)
            map)
        CoordMap.empty data
    in
    let map =
      CoordMap.map (fun x -> x mod 2 = 1) map
      |> CoordMap.filter (fun _ x -> x)
      |> CoordMap.bindings |> List.map fst |> CoordSet.of_list
    in
    let neki = apply step map 100 in
    neki |> CoordSet.cardinal |> string_of_int
end
