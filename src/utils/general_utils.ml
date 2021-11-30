let bin_to_decimal l =
  let rec bin_to_s a = function
    | [] -> a
    | x :: xs -> if x then bin_to_s ((2 * a) + 1) xs else bin_to_s (2 * a) xs
  in
  bin_to_s 0 l

let rec apply f x0 n = if n <= 0 then x0 else apply f (f x0) (n - 1)

let to2_arr l = l |> List.map Array.of_list |> Array.of_list

let to2_list l = l |> Array.map Array.to_list |> Array.to_list
