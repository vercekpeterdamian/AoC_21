open Solvers.Signature
open Utils.List_utils
open Utils.String_utils
module IntMap = Map.Make (Int)
module IntSet = Set.Make (Int)

module Solver : Solver = struct
  type requirement = int * int

  type field = {
    ind : int;
    interesting : bool;
    name : string;
    r1 : requirement;
    r2 : requirement;
  }

  type ticket = int list

  type note = {
    requ_fields : field list;
    my_ticket : ticket;
    nearby_tickets : ticket list;
  }

  let parse_filed i s =
    let[@warning "-8"] [ name; nums ] = String.split_on_char ':' s in
    Scanf.sscanf nums " %d-%d or %d-%d" (fun a b c d ->
        {
          ind = i;
          interesting = String.startswith "departure" name;
          name;
          r1 = (a, b);
          r2 = (c, d);
        })

  let parse_ticket s = s |> String.split_on_char ',' |> List.map int_of_string

  let is_valid_for_field num { r1 = a, b; r2 = c, d; _ } =
    (a <= num && num <= b) || (c <= num && num <= d)

  let parse_data s =
    let[@warning "-8"] [ fields; [ _; my ]; (_ :: nearby_tickets) ] =
      s |> String.split_on_char '\n' |> List.group_list |> List.map List.rev
    in
    {
      requ_fields = List.mapi parse_filed fields;
      my_ticket = parse_ticket my;
      nearby_tickets = List.map parse_ticket nearby_tickets;
    }

  let solve2 data =
    let initial_set =
      IntSet.of_list (List.map (fun x -> x.ind) data.requ_fields)
    in
    let possible_fields = Array.make (List.length data.my_ticket) initial_set in
    List.iter
      (fun ticket ->
        (* for every ticket *)
        List.iteri
          (fun field_ind ticket_field ->
            let r =
              List.fold_left
                (fun acc rule ->
                  if is_valid_for_field ticket_field rule then
                    IntSet.add rule.ind acc
                  else acc)
                IntSet.empty data.requ_fields
            in
            if not @@ IntSet.is_empty r then
              possible_fields.(field_ind) <-
                IntSet.inter possible_fields.(field_ind) r)
          ticket)
      data.nearby_tickets;
    let possible_fields = Array.mapi (fun i x -> (i, x)) possible_fields in
    Array.sort
      (fun x y ->
        Stdlib.compare (IntSet.cardinal @@ snd x) (IntSet.cardinal @@ snd y))
      possible_fields;
    let field_a = Array.of_list data.requ_fields in
    let m_a = Array.of_list data.my_ticket in
    let r =
      Array.fold_left
        (fun (taken, prod) (original_i, s) ->
          let dif = IntSet.diff s taken in
          let ind = IntSet.choose dif in
          let field = field_a.(ind) in
          let prod = prod * if field.interesting then m_a.(original_i) else 1 in
          (IntSet.add ind taken, prod))
        (IntSet.empty, 1) possible_fields
    in
    snd r

  let naloga1 data =
    let data = parse_data data in
    List.map
      (fun ticket ->
        (* Ticket *)
        List.sum
          (List.filter
             (fun number ->
               not
                 (List.fold_left ( || ) false
                    (List.map (is_valid_for_field number) data.requ_fields)))
             ticket))
      data.nearby_tickets
    |> List.sum |> string_of_int

  let naloga2 data _part1 =
    let data = parse_data data in
    let sol = solve2 data in
    string_of_int sol
end
