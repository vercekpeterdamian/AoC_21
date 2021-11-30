open Solvers.Signature
open Utils.List_utils

module Solver : Solver = struct
  type direction = N | E | S | W

  let dir_to_int = function N -> 0 | E -> 1 | S -> 2 | W -> 3

  let directions = [ N; E; S; W ]

  let dir_to_moves = function
    | N -> (0, 1)
    | E -> (1, 0)
    | S -> (0, -1)
    | W -> (-1, 0)

  let cycle_dir n current_dir =
    List.nth directions ((dir_to_int current_dir + n) mod List.length directions)

  type ship = {
    direction : direction;
    location : int * int;
    waypoint : int * int;
  }

  let inital_ship = { direction = E; location = (0, 0); waypoint = (10, 1) }

  type action = Move of direction * int | Rotate of int | Forward of int

  let move dir (x, y) n =
    let dx, dy = dir_to_moves dir in
    (x + (dx * n), y + (dy * n))

  let move_ship ship dir n = { ship with location = move dir ship.location n }

  let step ship action =
    match action with
    | Rotate i -> { ship with direction = cycle_dir i ship.direction }
    | Forward i -> move_ship ship ship.direction i
    | Move (dir, i) -> move_ship ship dir i

  let rec rotate (x, y) = function 0 -> (x, y) | n -> rotate (y, -x) (n - 1)

  let step_way ({ location = x, y; _ } as ship) action =
    match action with
    | Move (dir, i) -> { ship with waypoint = move dir ship.waypoint i }
    | Forward i ->
        let dx, dy = ship.waypoint in
        { ship with location = (x + (dx * i), y + (dy * i)) }
    | Rotate i -> { ship with waypoint = rotate ship.waypoint i }

  let parse_line x =
    Scanf.sscanf x "%c%d" (fun c n ->
        match c with
        | 'N' -> Move (N, n)
        | 'S' -> Move (S, n)
        | 'E' -> Move (E, n)
        | 'W' -> Move (W, n)
        | 'L' -> Rotate (-(n / 90) + 4)
        | 'R' -> Rotate (n / 90)
        | 'F' -> Forward n
        | _ -> failwith "Invalid input")

  let parse_data d = List.map parse_line (List.lines d)

  let naloga1 data =
    let instructions = parse_data data in
    let { location = x, y; _ } = List.fold_left step inital_ship instructions in
    abs x + abs y |> string_of_int

  let naloga2 data _part1 =
    let instructions = parse_data data in
    let { location = x, y; _ } =
      List.fold_left step_way inital_ship instructions
    in
    abs x + abs y |> string_of_int
end
