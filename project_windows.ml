let preberi_datoteko ime_datoteke =
  let chan = open_in ime_datoteke in
  let vsebina = really_input_string chan (in_channel_length chan) in
  close_in chan;
  vsebina

let izpisi_datoteko ime_datoteke vsebina =
  let chan = open_out ime_datoteke in
  output_string chan vsebina;
  close_out chan

module List = struct
  include List

  let int_list l = List.map int_of_string l

  let sum l =
    let rec sum' a = function [] -> a | x :: xs -> sum' (a + x) xs in
    sum' 0 l

  let lines = String.split_on_char '\n'
end

module type Solver = sig
  val naloga1 : string -> string

  val naloga2 : string -> string -> string
end

module Solver0 : Solver = struct
  let cost_fun x = (x / 3) - 2

  let rec full_cost x =
    let c_cost = cost_fun x in
    if c_cost <= 0 then 0 else c_cost + full_cost c_cost

  let naloga1 data =
    let lines = List.lines data in
    lines |> List.int_list
    |> List.fold_left (fun s x -> s + cost_fun x) 0
    |> string_of_int

  let naloga2 data _part1 =
    data |> List.lines |> List.int_list |> List.map full_cost |> List.sum
    |> string_of_int
end



(* Tukaj re-definirajte funkcijo naloga1 in naloga2 *)
module Solver1 : Solver = struct
  let naloga1 data = 
    let lines = List.int_list (List.lines data) in
    let rec aux acc =
      function
      | [] -> string_of_int acc
      | g1 :: g2 :: r when g1 < g2 -> aux (acc + 1) (g2 :: r)
      | g :: r -> aux acc r
    in
    aux 0 lines

  let naloga2 data _part1= 
    let lines = List.int_list (List.lines data) in
    let rec aux acc = 
      function
      | [] -> string_of_int acc
      | g1 :: g2 :: g3 :: g4 :: r when (g1 + g2 + g3) < (g2 + g3 + g4) -> aux (acc + 1) (g2 :: g3 :: g4 :: r)
      | _ :: r -> aux acc r
    in
    aux 0 lines
end



module Solver2 : Solver = struct
  let naloga1 data =
    let lines = List.lines data in
    let rec aux globina naravnost = function
      | [] -> string_of_int (globina * naravnost)
      | g :: r ->
        let navodilo = String.split_on_char ' ' g in
        match navodilo with
        | "forward" :: x :: _ -> aux globina (naravnost + int_of_string x) r
        | "up" :: x :: _ -> aux (globina - int_of_string x) naravnost r
        | "down" :: x :: _ -> aux (globina + int_of_string x) naravnost r 
        | _ -> failwith "nekaj ne deluje"
    in
    aux 0 0 lines


  let naloga2 data _part1 =
    let lines = List.lines data in
    let rec aux globina naravnost smer = function
      | [] -> string_of_int (globina * naravnost)
      | g :: r ->
        let navodilo = String.split_on_char ' ' g in
        match navodilo with
        | "up" :: x :: _ -> aux globina naravnost (smer - int_of_string x) r
        | "down" :: x :: _ -> aux globina naravnost (smer + int_of_string x) r
        | "forward" :: x :: _ -> aux (globina + smer * (int_of_string x)) (naravnost + int_of_string x) smer r
        | _ -> failwith "Cry about it"
    in
    aux 0 0 0 lines
    

end



module Solver3: Solver = struct
  (* https://caml.inria.fr/pub/old_caml_site/FAQ/FAQ_EXPERT-eng.html#strings *)
  let explode s =
    let rec exp i l =
     if i < 0 then l else exp (i - 1) (s.[i] :: l) in
    exp (String.length s - 1) []

  let rec sestej_seznama sez1 sez2 =
    match (sez1, sez2) with
    | ([], []) -> []
    | (g1 :: r1, g2 :: r2) -> (g1 +. g2) :: sestej_seznama r1 r2
    | _ -> failwith "SEZNAMA NISTA ENAKO DOLGA!!"

  let rec primerjaj_v_bin (pr, sez) =
    match sez with
    | [] -> []
    | a :: r -> (if 2. *. a > pr then 1. else 0.) :: primerjaj_v_bin (pr, r)

  let sez_bin_v_flt sez =
    let rec aux acc i = function
      | [] -> acc
      | 1. :: rr -> aux (acc +. 2. ** i) (i +. 1.) rr
      | _ :: rr -> aux acc (i +. 1.) rr
    in
    aux 0. 0. sez

  let rec get_gamma counter_list ct = function
    | [] -> ((float_of_int ct), counter_list)
    | st :: rep -> 
      let sezc = st
        |> explode
        |> List.map (fun x -> if x = '1' then 1. else 0.)
      in
      if counter_list = [] then get_gamma sezc (ct + 1) rep
      else get_gamma (sestej_seznama counter_list sezc) (ct + 1) rep

  let naloga1 data =
    let lines = List.lines data in 
    let gamma = lines 
      |> get_gamma [] 0 
      |> primerjaj_v_bin
      |> List.rev 
      |> sez_bin_v_flt 
    in
    string_of_int (int_of_float (gamma *. ( 2. ** 12. -. 1. -. gamma)))


  let kisik_krit (ct, x) =
    if 2 * x >= ct then '1' else '0'

  let ogljik_krit (ct, x) =
    if 2 * x < ct then '1' else '0'

  let rec prestej_enice acc cot = function
    | [] -> (acc, cot)
    | x :: rep ->
      if (List.nth x 0)  = '1' then prestej_enice (acc + 1) (cot + 1) rep
      else prestej_enice (acc + 1) cot rep

  let rec sestavi_s_kriterijem acc dolz cot general krit i sez =
    if i = 0 then
      let hocem = general (prestej_enice 0 0 sez) in
      sestavi_s_kriterijem acc dolz cot general hocem (i + 1) sez
    else
    if i < 12 then
      match sez with
      | x :: [] when acc = [] -> x
      | [] -> 
        let novek = general (dolz, cot) in
        sestavi_s_kriterijem [] 0 0 general novek (i + 1) acc
      | x :: rep ->
        if (List.nth x (i - 1)) <> krit then
          sestavi_s_kriterijem acc dolz cot general krit i rep
        else
          if (List.nth x i) = '1' then
            sestavi_s_kriterijem (x :: acc) (dolz + 1) (cot + 1) general krit i rep
          else
            sestavi_s_kriterijem (x :: acc) (dolz + 1) cot general krit i rep
    else
      match sez with
      | [] -> failwith "Predam se"
      | x :: rep ->
        if (List.nth x 11) = krit then x
        else sestavi_s_kriterijem [] 0 0 general krit 12 rep


    
  let naloga2 data _part1 =
      let lines = data 
        |> List.lines
        |> List.map explode
      in
      let kisik = lines
        |> sestavi_s_kriterijem [] 0 0 kisik_krit '0' 0 
        |> List.rev
        |> List.map (fun x -> if x = '1' then 1. else 0.)
        |> sez_bin_v_flt
      in
      let ogljik = lines
        |> sestavi_s_kriterijem [] 0 0 ogljik_krit '0' 0
        |> List.rev
        |> List.map (fun x -> if x = '1' then 1. else 0.)
        |> sez_bin_v_flt
      in
      (kisik *. ogljik) |> int_of_float |> string_of_int
end



module Solver4: Solver = struct
  let naloga1 data = ""

  let naloga2 data _part1 = ""
end



(* Poženemo zadevo *)
let choose_solver : string -> (module Solver) = function
  | "0" -> (module Solver0)
  | "1" -> (module Solver1)
  | "2" -> (module Solver2)
  | "3" -> (module Solver3)
  | "4" -> (module Solver4)
  | _ -> failwith "Ni še rešeno"

let main () =
  let day = Sys.argv.(1) in
  print_endline ("Solving DAY: " ^ day);
  let (module Solver) = choose_solver day in
  let input_data = preberi_datoteko ("data/day_" ^ day ^ ".in") in
  let p1_start = Sys.time () in
  let part1 = Solver.naloga1 input_data in
  let t1_time = Sys.time () -. p1_start in
  print_endline "PART 1:";
  print_endline part1;
  print_endline ("Taken: " ^ string_of_float t1_time ^ "s");
  let p2_start = Sys.time () in
  let part2 = Solver.naloga2 input_data part1 in
  let t2_time = Sys.time () -. p2_start in
  print_endline "PART 2:";
  print_endline part2;
  print_endline ("Taken: " ^ string_of_float t2_time ^ "s");
  print_endline ("Total: " ^ string_of_float (t1_time +. t2_time) ^ "s");
  izpisi_datoteko ("out/day_" ^ day ^ "_1.out") part1;
  izpisi_datoteko ("out/day_" ^ day ^ "_2.out") part2;
  ()

let _ = main ()
