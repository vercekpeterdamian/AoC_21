open Solvers.Signature
open Utils.List_utils
module IntMap = Map.Make (Int)
module IntSet = Set.Make (Int)

module Solver : Solver = struct
  type slice = bool Array.t Array.t

  type cube = slice Array.t

  (*
  let count_slice slice =
    Array.fold_left
      (fun s a -> Array.fold_left (fun s' x -> if x then 1 + s' else s') s a)
      0 slice

  let count (cube : cube) =
    Array.fold_left (fun s a -> s + count_slice a) 0 cube


  let string_of_row l =
    Array.map (fun x -> if x then "#" else ".") l
    |> Array.to_list |> String.concat ""

  let print_slice slice =
    slice |> Array.map string_of_row |> Array.to_list |> String.concat "\n"

  let print_cube cube =
    Printf.printf "\nXXXXXXXXXX\n";
    Array.iter
      (fun x -> Printf.printf "---------\n%s\n---------\n" (print_slice x))
      cube;
    Printf.printf "XXXXXXXXXX\n"
*)
  let parse_data n s : cube =
    let lines = List.lines s |> List.map List.list_of_string in
    let slice =
      List.map (List.map (( = ) '#')) lines
      |> List.map Array.of_list |> Array.of_list
    in
    let size = (2 * n) + Array.length slice in
    let rtr =
      Array.init ((2 * n) + 1) (fun _ -> Array.make_matrix size size false)
    in
    let w = Array.length slice in
    for x = 0 to w - 1 do
      for y = 0 to w - 1 do
        rtr.(n).(n + x).(n + y) <- slice.(x).(y)
      done
    done;
    rtr

  let empty a z x y = Array.init z (fun _ -> Array.make_matrix x y a)

  let inside a b c = a <= b && b <= c

  let is_change (dx, dy, dz) = not (List.for_all (( = ) 0) [ dx; dy; dz ])

  let process (state : cube) : cube =
    let m_z = Array.length state in
    let m_x = Array.length state.(0) in
    let m_y = Array.length state.(0).(0) in
    let d_neigbours = List.cartesian3 [ -1; 0; 1 ] [ -1; 0; 1 ] [ -1; 0; 1 ] in
    let all_neighbours (z, x, y) =
      List.fold_left
        (fun acc ((dz, dx, dy) as ch) ->
          if
            is_change ch
            && List.for_all
                 (fun (n, lim) -> inside 0 n (lim - 1))
                 [ (z + dz, m_z); (x + dx, m_x); (y + dy, m_y) ]
          then (z + dz, x + dx, y + dy) :: acc
          else acc)
        [] d_neigbours
    in
    let count_neig a l =
      List.sum
        (List.map
           (fun (z, x, y) -> if a.(z).(x).(y) then 1 else 0)
           (all_neighbours l))
    in
    let rtr = empty false m_z m_x m_y in
    for z = 0 to m_z - 1 do
      for x = 0 to m_x - 1 do
        for y = 0 to m_y - 1 do
          let c = count_neig state (z, x, y) in
          rtr.(z).(x).(y) <-
            (if state.(z).(x).(y) then 2 <= c && c <= 3 else c = 3)
        done
      done
    done;
    rtr

  let rec run cube = function
    | 0 -> cube
    | n ->
        (* print_cube cube; *)
        run (process cube) (n - 1)

  type cube4 = cube Array.t

  let count_slice slice =
    Array.fold_left
      (fun s a -> Array.fold_left (fun s' x -> if x then 1 + s' else s') s a)
      0 slice

  let count (cube : cube) =
    Array.fold_left (fun s a -> s + count_slice a) 0 cube

  let count4 (c : cube4) = Array.fold_left (fun s a -> s + count a) 0 c

  let parse_data4 n s : cube4 =
    let lines = List.lines s |> List.map List.list_of_string in
    let slice =
      List.map (List.map (( = ) '#')) lines
      |> List.map Array.of_list |> Array.of_list
    in
    let size = (2 * n) + Array.length slice in
    let rtr =
      Array.init
        ((2 * n) + 1)
        (fun _ ->
          Array.init ((2 * n) + 1) (fun _ -> Array.make_matrix size size false))
    in
    let w = Array.length slice in
    for x = 0 to w - 1 do
      for y = 0 to w - 1 do
        rtr.(n).(n).(n + x).(n + y) <- slice.(x).(y)
      done
    done;
    rtr

  let empty4 a t z x y =
    Array.init t (fun _ -> Array.init z (fun _ -> Array.make_matrix x y a))

  let is_change4 (dt, dz, dx, dy) =
    not (List.for_all (( = ) 0) [ dt; dx; dy; dz ])

  let process4 (state : cube4) : cube4 =
    let m_z = Array.length state in
    let m_x = Array.length state.(0) in
    let m_y = Array.length state.(0).(0) in
    let m_t = Array.length state.(0).(0).(0) in
    let d_neigbours =
      List.cartesian4 [ -1; 0; 1 ] [ -1; 0; 1 ] [ -1; 0; 1 ] [ -1; 0; 1 ]
    in
    let all_neighbours (z, x, y, t) =
      List.fold_left
        (fun acc ((dz, dx, dy, dt) as ch) ->
          if
            is_change4 ch
            && List.for_all
                 (fun (n, lim) -> inside 0 n (lim - 1))
                 [ (z + dz, m_z); (x + dx, m_x); (y + dy, m_y); (t + dt, m_t) ]
          then (z + dz, x + dx, y + dy, t + dt) :: acc
          else acc)
        [] d_neigbours
    in
    let count_neig a l =
      List.sum
        (List.map
           (fun (z, x, y, t) -> if a.(z).(x).(y).(t) then 1 else 0)
           (all_neighbours l))
    in
    let rtr = empty4 false m_z m_x m_y m_t in
    for z = 0 to m_z - 1 do
      for x = 0 to m_x - 1 do
        for y = 0 to m_y - 1 do
          for t = 0 to m_t - 1 do
            let c = count_neig state (z, x, y, t) in
            rtr.(z).(x).(y).(t) <-
              (if state.(z).(x).(y).(t) then 2 <= c && c <= 3 else c = 3)
          done
        done
      done
    done;
    rtr

  let rec run4 cube = function
    | 0 -> cube
    | n ->
        (* print_cube cube; *)
        run4 (process4 cube) (n - 1)

  let naloga1 data =
    let state = parse_data 6 data in
    count (run state 6) |> string_of_int

  let naloga2 data _part1 =
    let state = parse_data4 6 data in
    count4 (run4 state 6) |> string_of_int
end
