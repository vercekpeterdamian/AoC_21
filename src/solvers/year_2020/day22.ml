open Solvers.Signature
open Utils.List_utils
open Utils.String_utils

module Solver : Solver = struct
  type player = { deck : int Queue.t }

  type winner = First of player * player | Second of player * player

  module StringSet = Set.Make (String)

  module ListPSet = Set.Make (struct
    type t = int list * int list

    let rec comparel l1 l2 =
      match (l1, l2) with
      | [], [] -> 0
      | [], _ -> -1
      | _, [] -> 1
      | h :: t, hh :: tt ->
          if h > hh then 1 else if h < hh then -1 else comparel t tt

    let compare (l1, l2) (ll1, ll2) =
      let c1 = comparel l1 ll1 in
      if c1 = 0 then comparel l2 ll2 else c1
  end)

  exception EndGame

  let to_player l =
    {
      deck =
        l |> List.rev |> List.tl |> List.map int_of_string |> List.to_seq
        |> Queue.of_seq;
    }

  let parse_data d =
    let[@warning "-8"] [ p1; p2 ] = d |> List.lines |> List.group_list in
    (p1 |> to_player, p2 |> to_player)

  let score { deck } =
    deck |> Queue.to_seq |> List.of_seq |> List.rev
    |> List.mapi (fun i x ->
           let k = (i + 1) * x in
           k)
    |> List.fold_left ( + ) 0

  let rec play ({ deck = deck1 } as p1) ({ deck = deck2 } as p2) =
    match (Queue.is_empty deck1, Queue.is_empty deck2) with
    | true, _ -> (-1, score p2)
    | _, true -> (score p1, -1)
    | _ ->
        let t1 = Queue.pop deck1 in
        let t2 = Queue.pop deck2 in
        if t1 > t2 then (
          Queue.add t1 deck1;
          Queue.add t2 deck1;
          play p1 p2)
        else (
          Queue.add t2 deck2;
          Queue.add t1 deck2;
          play p1 p2)

  (* Different implementations of history tracking, fun to see the time difference *)
  (*
  let rec play_improved history ({ deck = deck1 } as p1)
      ({ deck = deck2 } as p2) =
    match
      StringSet.find_opt
        ((deck1 |> List.of_queue) ^ ";" ^ (deck2 |> List.of_queue))
        history
    with
    | Some _ -> raise EndGame
    | None -> (
        match (Queue.is_empty deck1, Queue.is_empty deck2) with
        | true, _ -> Second (p1, p2)
        | _, true -> First (p1, p2)
        | _ -> (
            try
              let history =
                StringSet.add
                  ((deck1 |> List.of_queue) ^ ";" ^ (deck2 |> List.of_queue))
                  history
              in
              let t1 = Queue.pop deck1 in
              let t2 = Queue.pop deck2 in
              if t1 <= Queue.length deck1 && t2 <= Queue.length deck2 then (
                let q1 =
                  deck1 |> Queue.to_seq |> List.of_seq |> List.take t1
                  |> List.to_seq |> Queue.of_seq
                in
                let q2 =
                  deck2 |> Queue.to_seq |> List.of_seq |> List.take t2
                  |> List.to_seq |> Queue.of_seq
                in
                match
                  play_improved StringSet.empty { deck = q1 } { deck = q2 }
                with
                | First _ ->
                    Queue.add t1 deck1;
                    Queue.add t2 deck1;
                    play_improved history p1 p2
                | Second _ ->
                    Queue.add t2 deck2;
                    Queue.add t1 deck2;
                    play_improved history p1 p2)
              else
                (* Normal *)
                let history =
                  StringSet.add
                    ((deck1 |> List.of_queue) ^ ";" ^ (deck2 |> List.of_queue))
                    history
                in
                if t1 > t2 then (
                  Queue.add t1 deck1;
                  Queue.add t2 deck1;
                  play_improved history p1 p2)
                else (
                  Queue.add t2 deck2;
                  Queue.add t1 deck2;
                  play_improved history p1 p2)
            with EndGame -> First (p1, p2)))
*)
  (*
  let rec play_improved1 history ({ deck = deck1 } as p1)
      ({ deck = deck2 } as p2) =
    match
      Hashtbl.find_opt history
        ((deck1 |> List.of_queue2) @ (deck2 |> List.of_queue2))
    with
    | Some _ -> raise EndGame
    | None -> (
        match (Queue.is_empty deck1, Queue.is_empty deck2) with
        | true, _ -> Second (p1, p2)
        | _, true -> First (p1, p2)
        | _ -> (
            try
              Hashtbl.add history
                ((deck1 |> List.of_queue2) @ (deck2 |> List.of_queue2))
                true;
              let t1 = Queue.pop deck1 in
              let t2 = Queue.pop deck2 in
              if t1 <= Queue.length deck1 && t2 <= Queue.length deck2 then (
                (* Yea sure, just use cicrular queue and copy first n elements *)
                let q1 =
                  deck1 |> Queue.to_seq |> List.of_seq |> List.take t1
                  |> List.to_seq |> Queue.of_seq
                in
                let q2 =
                  deck2 |> Queue.to_seq |> List.of_seq |> List.take t2
                  |> List.to_seq |> Queue.of_seq
                in
                match
                  play_improved1 (Hashtbl.create 200) { deck = q1 }
                    { deck = q2 }
                with
                | First _ ->
                    Queue.add t1 deck1;
                    Queue.add t2 deck1;
                    play_improved1 history p1 p2
                | Second _ ->
                    Queue.add t2 deck2;
                    Queue.add t1 deck2;
                    play_improved1 history p1 p2)
              else (
                (* Normal *)
                Hashtbl.add history
                  ((deck1 |> List.of_queue2) @ (deck2 |> List.of_queue2))
                  true;
                if t1 > t2 then (
                  Queue.add t1 deck1;
                  Queue.add t2 deck1;
                  play_improved1 history p1 p2)
                else (
                  Queue.add t2 deck2;
                  Queue.add t1 deck2;
                  play_improved1 history p1 p2))
            with EndGame -> First (p1, p2)))
*)
  let rec play_improved history ({ deck = deck1 } as p1)
      ({ deck = deck2 } as p2) =
    match
      ListPSet.find_opt
        (deck1 |> List.of_queue2, deck2 |> List.of_queue2)
        history
    with
    | Some _ -> raise EndGame
    | None -> (
        match (Queue.is_empty deck1, Queue.is_empty deck2) with
        | true, _ -> Second (p1, p2)
        | _, true -> First (p1, p2)
        | _ -> (
            try
              let history =
                ListPSet.add
                  (deck1 |> List.of_queue2, deck2 |> List.of_queue2)
                  history
              in
              let t1 = Queue.pop deck1 in
              let t2 = Queue.pop deck2 in
              if t1 <= Queue.length deck1 && t2 <= Queue.length deck2 then (
                let q1 =
                  deck1 |> Queue.to_seq |> List.of_seq |> List.take t1
                  |> List.to_seq |> Queue.of_seq
                in
                let q2 =
                  deck2 |> Queue.to_seq |> List.of_seq |> List.take t2
                  |> List.to_seq |> Queue.of_seq
                in
                match
                  play_improved ListPSet.empty { deck = q1 } { deck = q2 }
                with
                | First _ ->
                    Queue.add t1 deck1;
                    Queue.add t2 deck1;
                    play_improved history p1 p2
                | Second _ ->
                    Queue.add t2 deck2;
                    Queue.add t1 deck2;
                    play_improved history p1 p2)
              else
                (* Normal *)
                let history =
                  ListPSet.add
                    (deck1 |> List.of_queue2, deck2 |> List.of_queue2)
                    history
                in
                if t1 > t2 then (
                  Queue.add t1 deck1;
                  Queue.add t2 deck1;
                  play_improved history p1 p2)
                else (
                  Queue.add t2 deck2;
                  Queue.add t1 deck2;
                  play_improved history p1 p2)
            with EndGame -> First (p1, p2)))

  let naloga1 data =
    let p1, p2 = parse_data data in
    let s1, s2 = play p1 p2 in
    max s1 s2 |> string_of_int

  let naloga2 data _part1 =
    let p1, p2 = parse_data data in
    let w = play_improved ListPSet.empty p1 p2 in
    match w with First (x, _) | Second (_, x) -> score x |> string_of_int
end
