open Day

module Types = struct
  let pp_array = Array_utils.pp_array

  type arr = int array [@@deriving show]
  type input = arr list [@@deriving show]
  type output = int [@@deriving show]
end

module Parsing = struct
  open Angstrom
  open Parsing

  let line = many1 digit >>| Array.of_list
  let input = sep_by end_of_line line
end

module Solving = struct

  (* Find the max element in a sub array of a starting at pos of length size.
     For example [max_id_borned ~pos:2 ~size:2 [|1;2;3;4;5|]] will search the
     max in [[|3;4|]] and return (4, 3) where 3 is 4's ID in the original array.
  *)
  let max_id_borned ~pos ~size a =
    let len = Array.length a in
    assert (pos >= 0 && pos + size <= len);
    Array.sub a pos size
    |> Array_utils.find_max_id
    |> fun (m, i) -> (m, pos + i)

  (* Compute values for max_id_borned with a starting position and the number
     of element starting from the right we want to ignore.
     For example [max_id_pos [|1;2;3;4;5|] 0 2] will search the max in
     [|1;2;3;|]
  *)
  let max_id_pos ~pos ~right a =
    let size = Array.length a - right - pos in
    max_id_borned ~pos ~size a

  (* Find the n max distincts elements in the array in the original order, to
     to form the biggest number of size n. *)
  let find_power batteries size =
    let rec aux power pos n =
      if n = 0 then power
      else
        let max, id = max_id_pos ~pos ~right:(n - 1) batteries in
        let power = (power * 10) + max in
        aux power (id + 1) (n - 1)
    in
    aux 0 0 size

  let part_aux banks size =
    List.fold_left (fun acc bank ->
        acc + find_power bank size
      ) 0 banks

  let part1 (input : Types.input) : Types.output =
    part_aux input 2

  let part2 (input : Types.input) : Types.output =
    part_aux input 12
end

module Today = MakeDay (Types) (Parsing) (Solving)

let () = Today.run_all ()
