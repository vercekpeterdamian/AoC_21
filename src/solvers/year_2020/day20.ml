open Solvers.Signature
open Utils.List_utils
open Utils.String_utils
open Utils.General_utils

type edge = bool list

type passport = bool list list

type rotation = { flip : bool; rot : int }

type tile = {
  id : int;
  passport : passport;
  rotation : rotation;
  top : edge;
  bottom : edge;
  left : edge;
  right : edge;
}

let print_tile tile =
  List.iter
    (fun x ->
      print_endline
      @@ String.concat "" (List.map (fun x -> if x then "#" else ".") x))
    tile.passport

let rotate l = List.transpose @@ List.rev l

let get_top l : bool list = List.hd l

let get_bot tile = get_top (List.rev tile)

let get_left tile = get_top (List.transpose tile)

let get_right tile = get_bot (List.transpose tile)

let generate_rotations id p =
  let simple =
    List.init 4 (fun i ->
        let pp = apply rotate p (i + 1) in
        {
          id;
          passport = pp;
          rotation = { flip = false; rot = i + 1 };
          top = get_top pp;
          bottom = get_bot pp;
          left = get_left pp;
          right = get_right pp;
        })
  in
  let flip =
    List.init 4 (fun i ->
        let pp = apply rotate (List.rev p) (i + 1) in
        {
          id;
          passport = pp;
          rotation = { flip = false; rot = i + 1 };
          top = get_top pp;
          bottom = get_bot pp;
          left = get_left pp;
          right = get_right pp;
        })
  in
  simple @ flip

let parse_tile s =
  let[@warning "-8"] (x :: rest) = s |> List.rev in
  let id = Scanf.sscanf x "Tile %d:" (fun n -> n) in
  let slice =
    rest |> List.map List.list_of_string |> List.map (List.map (( = ) '#'))
  in
  generate_rotations id slice

let parse s = List.group_list s |> List.map parse_tile |> List.map (fun x -> x)

let place (image : tile Array.t Array.t) (i, j) tile =
  (* Printf.printf "Placing: %d, %d: %d\n" i j tile.id; *)
  let top = i = 0 || image.(i - 1).(j).bottom = tile.top in
  let left = j = 0 || image.(i).(j - 1).right = tile.left in
  top && left

let rec solve image (i, j) width (tiles : tile list list) =
  if i = width then Some image
  else
    let rec test_full_tile available = function
      | [] -> None
      | t :: ts ->
          if place image (i, j) t then (
            image.(i).(j) <- t;
            let i', j' = if j + 1 = width then (i + 1, 0) else (i, j + 1) in
            match solve image (i', j') width available with
            | None -> test_full_tile available ts
            | x -> x)
          else test_full_tile available ts
    in
    let rec recuse fail = function
      | [] -> None
      | tile_s :: xs -> (
          let rest = fail @ xs in
          match test_full_tile rest tile_s with
          | None -> recuse (tile_s :: fail) xs
          | x -> x)
    in
    recuse [] tiles

let empty_image w =
  Array.make_matrix w w
    {
      id = -1;
      passport = [];
      rotation = { rot = -1; flip = true };
      top = [];
      bottom = [];
      left = [];
      right = [];
    }

let int_of_bool = function true -> 1 | false -> 0

let indices w img_size =
  let make_sublist start = List.init img_size (fun x -> x + start) in
  let n = img_size + 2 in
  List.concat (List.init w (fun x -> (x * n) + 1) |> List.map make_sublist)

module Solver : Solver = struct
  let img = ref (empty_image 0)

  let naloga1 data =
    let tiles = data |> List.lines |> parse in
    let w = floor (sqrt (List.length tiles |> float_of_int)) |> int_of_float in
    img := empty_image w;
    let _ = solve !img (0, 0) w tiles in
    let sol = !img in
    let k =
      sol.(0).(0).id * sol.(0).(w - 1).id * sol.(w - 1).(0).id
      * sol.(w - 1).(w - 1).id
    in
    k |> string_of_int

  let find_monster l (i1, i2, i3) img =
    let has_m (i, j) =
      List.for_all (fun ind -> img.(i).(j + ind)) i1
      && List.for_all (fun ind -> img.(i + 1).(j + ind)) i2
      && List.for_all (fun ind -> img.(i + 2).(j + ind)) i3
    in
    let copy = Array.copy img in
    let found = ref false in
    for i = 0 to Array.length img - 3 do
      for j = 0 to Array.length img.(0) - l do
        (* Printf.printf "%d %d" i j; *)
        if has_m (i, j) then (
          (* Printf.printf "here"; *)
          found := true;
          List.iter (fun ind -> copy.(i).(j + ind) <- false) i1;
          List.iter (fun ind -> copy.(i + 1).(j + ind) <- false) i2;
          List.iter (fun ind -> copy.(i + 2).(j + ind) <- false) i3)
      done
    done;
    (!found, copy)

  let naloga2 data _part1 =
    let tiles = data |> List.lines |> parse in
    let w = floor (sqrt (List.length tiles |> float_of_int)) |> int_of_float in
    let img_size =
      ((tiles |> List.hd |> List.hd).passport |> List.length) - 2
    in
    let size = img_size + 2 in
    let correct_image = Array.make_matrix (img_size * w) (img_size * w) false in
    let indices = indices w img_size in
    let image =
      Array.map (fun x -> Array.map (fun x -> x.passport |> to2_arr) x) !img
    in
    List.iteri
      (fun to_i_r fr_i_r ->
        List.iteri
          (fun to_i_c fr_i_c ->
            let tile = image.(fr_i_r / size).(fr_i_c / size) in
            correct_image.(to_i_r).(to_i_c) <-
              tile.(fr_i_r mod size).(fr_i_c mod size))
          indices)
      indices;
    let image = to2_list correct_image in
    let m1, m2, m3 =
      ("                  # ", "#    ##    ##    ###", " #  #  #  #  #  #   ")
    in
    let f s =
      List.mapi
        (fun i x -> if x = '#' then Some i else None)
        (List.list_of_string s)
      |> List.filter_map (fun x -> x)
    in
    let i1, i2, i3 = (f m1, f m2, f m3) in
    let images =
      generate_rotations (-1) image |> List.map (fun x -> x.passport |> to2_arr)
    in
    let[@warning "-8"] (Some img) =
      List.fold_left
        (fun c img ->
          let r, cpy = find_monster (String.length m1) (i1, i2, i3) img in
          if r then Some cpy else c)
        None images
    in
    let k =
      Array.fold_left
        (fun s a -> Array.fold_left (fun s' x -> if x then 1 + s' else s') s a)
        0 img
    in
    k |> string_of_int
end
