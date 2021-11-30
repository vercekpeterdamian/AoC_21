open Solvers.Signature
open Utils.List_utils

module Solver : Solver = struct
  type seat_type = Empty | Occupied

  type cell_type = Floor | Seat of seat_type

  (*
  let string_of_cell = function
    | Floor -> "."
    | Seat x -> ( match x with Empty -> "L" | Occupied -> "#" )


   let string_of_grid grid =
    let rows =
      Array.map
        (fun r -> String.concat "" (List.map string_of_cell (Array.to_list r)))
        grid
    in
    String.concat "\n" (Array.to_list rows)
*)

  type seat = {
    location : int * int;
    seat_type : seat_type;
    seat_neigh : (int * int) list; (* indices of seat neighbours *)
  }

  let parse_cell = function
    | 'L' -> Seat Empty
    | '#' -> Seat Occupied
    | '.' -> Floor
    | _ -> failwith "Invalid input"

  let is_occupied grid (i, j) =
    match grid.(i).(j) with Seat Occupied -> 1 | _ -> 0

  let parse_grid s =
    let base =
      s |> List.lines
      |> List.map List.list_of_string
      |> List.map (fun l -> Array.of_list @@ List.map parse_cell l)
      |> Array.of_list
    in
    let height = Array.length base in
    let width = Array.length base.(0) in
    let neighbours_i i j =
      List.filter
        (fun (i, j) -> (0 <= i && i < height) && 0 <= j && j < width)
        [
          (i - 1, j - 1);
          (i - 1, j);
          (i - 1, j + 1);
          (i, j - 1);
          (i, j + 1);
          (i + 1, j - 1);
          (i + 1, j);
          (i + 1, j + 1);
        ]
    in
    let cells = ref [] in
    for i = 0 to height - 1 do
      let line = base.(i) in
      for j = 0 to width - 1 do
        match line.(j) with
        | Seat s ->
            let neigh_i = neighbours_i i j in

            let neigh =
              List.filter_map
                (fun (i, j) ->
                  match base.(i).(j) with Seat _ -> Some (i, j) | _ -> None)
                neigh_i
            in
            let c = { location = (i, j); seat_type = s; seat_neigh = neigh } in
            cells := c :: !cells
        | Floor -> ()
      done
    done;
    (base, List.rev !cells)

  let step limit grid seats =
    (*
      print_endline "-------------------";
      print_endline @@ string_of_grid grid;
    *)
    let sums =
      List.map
        (fun c -> (c, List.sum (List.map (is_occupied grid) c.seat_neigh)))
        seats
    in
    let change, seats =
      List.fold_left_map
        (fun change (s, num) ->
          match s.seat_type with
          | Empty when num = 0 ->
              grid.(fst s.location).(snd s.location) <- Seat Occupied;
              (true, { s with seat_type = Occupied })
          | Occupied when num >= limit ->
              grid.(fst s.location).(snd s.location) <- Seat Empty;
              (true, { s with seat_type = Empty })
          | _ -> (change, s))
        false sums
    in
    (change, seats)

  let rec solve limit grid seats =
    let change, seats = step limit grid seats in
    match change with
    | true -> solve limit grid seats
    | false -> List.count_filter (fun s -> s.seat_type = Occupied) seats

  let solve2 limit grid seats =
    let height = Array.length grid in
    let width = Array.length grid.(0) in
    let rec find_visible (x, y) (dx, dy) =
      let x = x + dx in
      let y = y + dy in
      if (0 <= x && x < height) && 0 <= y && y < width then
        match grid.(x).(y) with
        | Seat _ -> Some (x, y)
        | _ -> find_visible (x, y) (dx, dy)
      else None
    in
    let seats =
      List.map
        (fun s ->
          let nei_opt =
            List.filter_map (find_visible s.location)
              [
                (-1, -1);
                (0, -1);
                (1, -1);
                (-1, 0);
                (1, 0);
                (-1, 1);
                (0, 1);
                (1, 1);
              ]
          in
          { s with seat_neigh = nei_opt })
        seats
    in
    print_endline "here";
    solve limit grid seats

  let naloga1 data =
    let grid, seats = parse_grid data in
    solve 4 grid seats |> string_of_int

  let naloga2 data _part1 =
    let grid, seats = parse_grid data in
    solve2 5 grid seats |> string_of_int
end
