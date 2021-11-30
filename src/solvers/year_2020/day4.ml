open Solvers.Signature
open Utils.List_utils
open Utils.Option_utils

module Solver : Solver = struct
  type so = string option

  type password = {
    byr : so;
    iyr : so;
    eyr : so;
    hgt : so;
    hcl : so;
    ecl : so;
    pid : so;
    cid : so;
  }

  let empty_password =
    {
      byr = None;
      iyr = None;
      eyr = None;
      hgt = None;
      hcl = None;
      ecl = None;
      pid = None;
      cid = None;
    }

  let is_valid p =
    let n =
      List.count false
        [
          p.byr = None;
          p.iyr = None;
          p.eyr = None;
          p.hgt = None;
          p.hcl = None;
          p.ecl = None;
          p.pid = None;
          p.cid = None;
        ]
    in
    n = 8 || (n = 7 && p.cid = None)

  let parse_field password v = function
    | "byr" -> { password with byr = Some v }
    | "iyr" -> { password with iyr = Some v }
    | "eyr" -> { password with eyr = Some v }
    | "hgt" -> { password with hgt = Some v }
    | "hcl" -> { password with hcl = Some v }
    | "ecl" -> { password with ecl = Some v }
    | "pid" -> { password with pid = Some v }
    | "cid" -> { password with cid = Some v }
    | x -> failwith ("Unmatched: " ^ x)

  let[@warning "-8"] split_field s =
    let [ field; v ] = String.split_on_char ':' s in
    (field, v)

  let mid a m b = a <= m && m <= b

  let[@warning "-27"] validate { byr; iyr; eyr; hgt; hcl; ecl; pid; cid = _cid }
      =
    let m = Option.map in
    List.for_all
      (Option.with_default false)
      [
        m (fun x -> mid 1920 (int_of_string x) 2002) byr;
        m (fun x -> mid 2010 (int_of_string x) 2020) iyr;
        m (fun x -> mid 2020 (int_of_string x) 2030) eyr;
        m
          (fun x ->
            let n, u = Scanf.sscanf x "%d%s" (fun x u -> (x, u)) in
            match u with
            | "cm" -> mid 150 n 193
            | "in" -> mid 59 n 76
            | _ -> false)
          hgt;
        m
          (fun x ->
            try Scanf.sscanf x "#%[0-9a-f]" (fun _ -> true) with _ -> false)
          hcl;
        m
          (fun x ->
            List.mem x [ "amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth" ])
          ecl;
        m (fun x -> String.length x == 9) pid;
      ]

  let to_password s =
    List.fold_left
      (fun p field ->
        let f, v = split_field field in
        parse_field p v f)
      empty_password s

  let password_list data =
    data |> List.lines |> List.groups |> List.map String.trim
    |> List.map (String.split_on_char ' ')
    |> List.map to_password

  let naloga1 data =
    data |> password_list |> List.count_filter is_valid |> string_of_int

  let naloga2 data _part1 =
    data |> password_list |> List.count_filter validate |> string_of_int
end
