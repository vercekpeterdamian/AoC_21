open Solvers.Signature
open Utils.List_utils

module Solver : Solver = struct
  module StringMap = Map.Make (String)
  module StringSet = Set.Make (String)

  type simple_bag = { modifier : string; color : string; full_s : string }

  type bag = {
    modifier : string;
    color : string;
    full_string : string;
    data : (simple_bag * int) list;
  }

  let parse_contets l =
    if l = " no other bags." then []
    else
      l |> String.split_on_char ','
      |> List.map (fun b ->
             Scanf.sscanf b " %d %s %s" (fun num modifier color ->
                 ({ color; modifier; full_s = modifier ^ " " ^ color }, num)))

  let parse_bag line =
    let modifier, color, rest =
      Scanf.sscanf line "%s %s %s contain %[^\n]" (fun modifier color _ rest ->
          (modifier, color, " " ^ rest))
    in
    {
      modifier;
      color;
      full_string = modifier ^ " " ^ color;
      data = parse_contets rest;
    }

  let rec make_graph g = function
    | [] -> g
    | { full_string; data; _ } :: xs ->
        let g =
          List.fold_left
            (fun g' (sb, _) ->
              StringMap.update sb.full_s
                (fun cur ->
                  match cur with
                  | None -> Some [ full_string ]
                  | Some x -> Some (full_string :: x))
                g')
            g data
        in
        make_graph g xs

  let rec traverse g start =
    match StringMap.find_opt start g with
    | None -> StringSet.empty
    | Some x ->
        List.fold_left StringSet.union (StringSet.of_list x)
          (List.map (traverse g) x)

  let naloga1 data =
    let bags = data |> List.lines |> List.map parse_bag in
    let g = make_graph StringMap.empty bags in
    traverse g "shiny gold" |> StringSet.cardinal |> string_of_int

  (* I really didn't wanna do toposort or use refs *)
  let count graph root =
    let rec count' memo root =
      match StringMap.find_opt root memo with
      | Some x -> (memo, x)
      | None ->
          let memo, r =
            List.fold_left
              (fun (m, s) (bag, num) ->
                let m, down = count' m bag.full_s in
                (m, s + (down * num)))
              (memo, 1) (StringMap.find root graph).data
          in
          (StringMap.add root r memo, r)
    in
    snd (count' StringMap.empty root)

  let naloga2 data _part1 =
    let bags =
      data |> List.lines |> List.map parse_bag
      |> List.fold_left
           (fun g s -> StringMap.add s.full_string s g)
           StringMap.empty
    in
    count bags "shiny gold" - 1 |> string_of_int
end
