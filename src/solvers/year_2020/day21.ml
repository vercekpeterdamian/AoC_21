open Solvers.Signature
open Utils.List_utils
open Utils.String_utils

module Solver : Solver = struct
  module StringSet = Set.Make (String)
  module StringMap = Map.Make (String)

  type line = { ingr : string list; allergens : string list }

  type state = {
    lines : line list;
    all_allergens : StringSet.t;
    all_ingredients : StringSet.t;
  }

  let parse_line l =
    let[@warning "-8"] [ ing; alg ] = String.split_on_char '(' l in
    let ing = String.trim ing in
    let l = String.length "contains " in
    {
      ingr = String.split_on_char ' ' ing;
      allergens =
        String.sub alg l (String.length alg - l - 1)
        |> String.split_on_char ',' |> List.map String.trim;
    }

  let parse_data data =
    let lines = List.map parse_line (data |> String.split_on_char '\n') in
    let all_ingredients =
      List.fold_left StringSet.union StringSet.empty
        (List.map (fun x -> StringSet.of_list x.ingr) lines)
    in
    let all_allergens =
      List.fold_left StringSet.union StringSet.empty
        (List.map (fun x -> StringSet.of_list x.allergens) lines)
    in
    { lines; all_allergens; all_ingredients }

  (*
     let sset s = s |> StringSet.to_seq |> List.of_seq |> String.concat ","
  *)
  let remove taken l =
    List.map
      (fun ((x, s), _) ->
        let ns = StringSet.remove taken s in
        ((x, ns), StringSet.cardinal ns))
      l
    |> List.sort (fun (_, a) (_, b) -> a - b)

  let rec process (taken : StringSet.t) acc = function
    | [] -> acc
    | ((name, poss), _) :: xs ->
        let poss = StringSet.diff poss taken in
        let _ = assert (StringSet.cardinal poss = 1) in
        let el = StringSet.min_elt poss in
        process (StringSet.add el taken) ((name, el) :: acc) (remove el xs)

  let naloga1 data =
    let data = parse_data data in
    let maybe_map =
      List.fold_left
        (fun m { ingr; allergens } ->
          let ing_set = StringSet.of_list ingr in
          List.fold_left
            (fun m al ->
              StringMap.update al
                (function
                  | None -> Some ing_set
                  | Some x -> Some (StringSet.inter x ing_set))
                m)
            m allergens)
        StringMap.empty data.lines
    in
    let sure_good =
      StringMap.fold
        (fun _ s1 acc -> StringSet.diff acc s1)
        maybe_map data.all_ingredients
    in
    let res =
      List.fold_left
        (fun s { ingr; _ } ->
          s
          + ((List.map (fun x -> StringSet.mem x sure_good)) ingr
            |> List.count true))
        0 data.lines
    in
    res |> string_of_int

  let naloga2 data _part1 =
    let data = parse_data data in
    let maybe_map =
      List.fold_left
        (fun m { ingr; allergens } ->
          let ing_set = StringSet.of_list ingr in
          List.fold_left
            (fun m al ->
              StringMap.update al
                (function
                  | None -> Some ing_set
                  | Some x -> Some (StringSet.inter x ing_set))
                m)
            m allergens)
        StringMap.empty data.lines
    in
    let u = StringMap.bindings maybe_map in
    let u = List.map (fun (a, b) -> ((a, b), StringSet.cardinal b)) u in
    let k = List.sort (fun (_, a) (_, b) -> a - b) u in
    List.iter (fun (_, x) -> Printf.printf "%d, " x) k;
    let res = process StringSet.empty [] k in
    let res =
      List.sort (fun (x, _) (y, _) -> Stdlib.compare x y) res |> List.map snd
    in
    String.concat "," res
end
