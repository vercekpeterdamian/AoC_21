open Solvers.Signature

let choose_solver year day : (module Solver) =
  match year with
  | "2020" -> (
      let open Solvers2020 in
      match day with
      | "0" -> (module Day0.Solver)
      | "1" -> (module Day1.Solver)
      | "2" -> (module Day2.Solver)
      | "3" -> (module Day3.Solver)
      | "4" -> (module Day4.Solver)
      | "5" -> (module Day5.Solver)
      | "6" -> (module Day6.Solver)
      | "7" -> (module Day7.Solver)
      | "8" -> (module Day8.Solver)
      | "9" -> (module Day9.Solver)
      | "10" -> (module Day10.Solver)
      | "11" -> (module Day11.Solver)
      | "12" -> (module Day12.Solver)
      | "13" -> (module Day13.Solver)
      | "14" -> (module Day14.Solver)
      | "15" -> (module Day15.Solver)
      | "16" -> (module Day16.Solver)
      | "17" -> (module Day17.Solver)
      | "18" -> (module Day18.Solver)
      | "19" -> (module Day19.Solver)
      | "20" -> (module Day20.Solver)
      | "21" -> (module Day21.Solver)
      | "22" -> (module Day22.Solver)
      | "23" -> (module Day23.Solver)
      | "24" -> (module Day24.Solver)
      | "25" -> (module Day25.Solver)
      | _ -> failwith "Ni še rešeno")
  | "2021" -> (
      let open Solvers2021 in
      match day with
      | "0" -> (module Day0.Solver)
      | _ -> failwith "Ni še rešeno")
  | _ -> failwith "Neveljavno leto"

let main () =
  let day = Sys.argv.(1) in
  let year = try Sys.argv.(2) with Invalid_argument _ -> "2021" in
  Printf.printf "%s\n" year;
  let folder = year ^ "/day_" in
  print_endline ("Solving DAY: " ^ day);
  let (module Solver) = choose_solver year day in
  let input_data =
    Utils.Files.preberi_datoteko ("data/" ^ folder ^ day ^ ".in")
  in
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
  Utils.Files.izpisi_datoteko ("out/" ^ folder ^ day ^ "_1.out") part1;
  Utils.Files.izpisi_datoteko ("out/" ^ folder ^ day ^ "_2.out") part2;
  ()

let _ = main ()
