module List = struct
  include Stdlib.List

  let int_list l = List.map int_of_string l

  let sum l =
    let rec sum' a = function [] -> a | x :: xs -> sum' (a + x) xs in
    sum' 0 l

  let multiply l = List.fold_left ( * ) 1 l

  let lines = String.split_on_char '\n'

  let rec make_patches rtr acc = function
    | [] -> List.rev (acc :: rtr)
    | "" :: (_ :: _ as rest) -> make_patches (acc :: rtr) "" rest
    | l :: rest -> make_patches rtr (acc ^ " " ^ l) rest

  let groups s = make_patches [] "" s

  let group_list l =
    let rec group_list' rtr acc = function
      | [] -> List.rev (acc :: rtr)
      | "" :: (_ :: _ as rest) -> group_list' (acc :: rtr) [] rest
      | l :: rest -> group_list' rtr (l :: acc) rest
    in
    group_list' [] [] l

  let with_index l = List.mapi (fun i x -> (i, x)) l

  let count a l = l |> List.filter (fun x -> x = a) |> List.length

  let list_of_string s = s |> String.to_seq |> List.of_seq

  let count_filter f l = l |> List.filter f |> List.length

  let count_filter2 f l =
    List.fold_right (fun x s -> match f x with true -> s + 1 | false -> s) l 0

  let maximum l = List.fold_left max (List.nth l 0) l

  let rec reduce fn = function
    | [] -> failwith "Empty list"
    | [ a ] -> a
    | x :: xs -> fn x (reduce fn xs)

  let reduce2 fn l =
    let rec reduce' acc fn l =
      match l with x :: xs -> reduce' (fn x acc) fn xs | [] -> acc
    in
    match l with [] -> failwith "Empty list" | x :: xs -> reduce' x fn xs

  let split_on_n l n =
    let rec split' acc i = function
      | [] -> (acc, [])
      | l' when i <= 0 -> (acc, l')
      | x :: xs -> split' (x :: acc) (i - 1) xs
    in
    let f, s = split' [] n l in
    (List.rev f, s)

  let rec map3 f l1 l2 l3 =
    match (l1, l2, l3) with
    | [], [], [] -> []
    | a1 :: l1, a2 :: l2, a3 :: l3 ->
        let r = f a1 a2 a3 in
        r :: map3 f l1 l2 l3
    | _ -> invalid_arg "List.map3"

  let cartesian l l' =
    List.concat (List.map (fun e -> List.map (fun e' -> (e, e')) l') l)

  let cartesian3 l1 l2 l3 =
    cartesian l1 (cartesian l2 l3) |> List.map (fun (z, (x, y)) -> (z, x, y))

  let cartesian4 l1 l2 l3 l4 =
    cartesian l1 (cartesian3 l2 l3 l4)
    |> List.map (fun (z, (x, y, t)) -> (z, x, y, t))

  let transpose (ls : 'a list list) : 'a list list =
    let rec transpose_rec acc = function
      | [] | [] :: _ -> List.rev acc
      | ls -> transpose_rec (List.map List.hd ls :: acc) (List.map List.tl ls)
    in
    transpose_rec [] ls

  let rec take n l =
    if n <= 0 then []
    else
      match l with
      | [] -> failwith "invalid len"
      | x :: xs -> x :: take (n - 1) xs

  let of_queue q =
    q |> Queue.to_seq |> List.of_seq |> List.map string_of_int
    |> String.concat ","

  let of_queue2 q = q |> Queue.to_seq |> List.of_seq

  let split l n =
    let rec aux acc n' l' =
      if n' <= 0 then (List.rev acc, l')
      else
        match l' with [] -> (acc, []) | x :: xs -> aux (x :: acc) (n' - 1) xs
    in
    aux [] n l

  let rotate l n =
    let f, s = split l n in
    s @ f

  let rotate1 = function [] -> [] | x :: xs -> xs @ [ x ]

  let take3 = function
    | a :: b :: c :: xs -> ([ a; b; c ], xs)
    | _ -> failwith "Error"
end
