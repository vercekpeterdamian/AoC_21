open Solvers.Signature
open Utils.List_utils
open Utils.String_utils

module Solver : Solver = struct
  module IntMap = Map.Make (Int)

  type matc =
    | Const of string
    | Or of int list * int list
    | Seq of int list
    | Eight of int
    | Eleven of int * int

  type rule = { name : int; matc : matc }

  type full_rule = { name : int; full : string }

  type graph = rule IntMap.t

  type state = { rules : rule list; words : string list; graph : graph }

  let parse_rule s =
    try
      Scanf.sscanf s "%d: \"%c\"" (fun n c ->
          { name = n; matc = Const (Char.escaped c) })
    with _ ->
      let[@warning "-8"] [ n; rest ] = String.split_on_char ':' s in
      if String.contains s '|' then
        let[@warning "-8"] [ r1; r2 ] =
          String.split_on_char '|' rest
          |> List.map (fun x ->
                 x |> String.trim |> String.split_on_char ' '
                 |> List.map int_of_string)
        in
        { name = int_of_string n; matc = Or (r1, r2) }
      else
        {
          name = int_of_string n;
          matc =
            Seq
              (rest |> String.trim |> String.split_on_char ' '
             |> List.map int_of_string);
        }

  let parse special s =
    let[@warning "-8"] [ rules; words ] =
      s |> String.split_on_char '\n' |> List.group_list
    in
    let rules = List.map parse_rule rules in
    let graph =
      rules
      |> List.map (fun (x : rule) -> (x.name, x))
      |> List.to_seq |> IntMap.of_seq
    in
    let graph =
      if special then
        graph
        |> IntMap.add 8 { name = 8; matc = Eight 42 }
        |> IntMap.add 11 { name = 11; matc = Eleven (42, 31) }
      else graph
    in
    { rules; words; graph }

  let traverse max_len (graph : graph) (root : int) =
    let rec make_full (memo : full_rule IntMap.t) (node : int) =
      match IntMap.find_opt node memo with
      | Some x ->
          (* Printf.printf "Memo: %d\n" node; *)
          (memo, x)
      | None ->
          let memo, r =
            let node = IntMap.find node graph in
            match node.matc with
            | Const s -> (memo, { name = node.name; full = s })
            | Or (r1, r2) ->
                let folder (memo, acc) x =
                  let memo, r = make_full memo x in
                  (memo, r.full :: acc)
                in
                let memo, s1 = List.fold_left folder (memo, []) r1 in
                let memo, s2 = List.fold_left folder (memo, []) r2 in
                ( memo,
                  {
                    name = node.name;
                    full =
                      "\\("
                      ^ (s1 |> List.rev |> String.concat "")
                      ^ "\\|"
                      ^ (s2 |> List.rev |> String.concat "")
                      ^ "\\)";
                  } )
            | Seq l ->
                let memo, full =
                  List.fold_left
                    (fun (memo, acc) x ->
                      let memo, r = make_full memo x in
                      (memo, r.full :: acc))
                    (memo, []) l
                in
                ( memo,
                  {
                    name = node.name;
                    full = full |> List.rev |> String.concat "";
                  } )
            | Eight x ->
                let memo, x = make_full memo x in
                (memo, { name = node.name; full = "\\(" ^ x.full ^ "\\)+" })
            | Eleven (x, y) ->
                let memo, x = make_full memo x in
                let memo, y = make_full memo y in
                let num = max_len in
                let full =
                  List.init num (fun n ->
                      "\\(\\("
                      ^ String.repeat x.full (n + 1)
                      ^ "\\)\\("
                      ^ String.repeat y.full (n + 1)
                      ^ "\\)\\)")
                in
                ( memo,
                  {
                    name = node.name;
                    full = "\\(" ^ (full |> String.concat "\\|") ^ "\\)";
                  } )
          in
          (IntMap.add node r memo, r)
    in
    let _, r = make_full IntMap.empty root in
    r

  let naloga1 data =
    let state = parse false data in
    let a = traverse 0 state.graph 0 in
    let regex = Str.regexp ("^" ^ a.full ^ "$") in
    state.words
    |> List.map (fun w -> Str.string_match regex w 0)
    |> List.count true |> string_of_int

  let naloga2 data _part1 =
    let state = parse true data in
    let a = traverse 10 state.graph 0 in
    let regex = Str.regexp ("^" ^ a.full ^ "$") in
    state.words
    |> List.map (fun w ->
           if Str.string_match regex w 0 && Str.match_end () = String.length w
           then true
           else false)
    |> List.count true |> string_of_int
end
