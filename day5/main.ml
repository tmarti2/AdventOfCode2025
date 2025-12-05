open Day

module Types = struct

  type input = (int * int) list * int list [@@deriving show]
  type output = int [@@deriving show]
end

module Parsing = struct
  open Angstrom
  open Parsing

  let range = lift3 (fun min _ max -> min,max) integer (char '-') integer
  let ranges = sep_by1 end_of_line range

  let ints = sep_by1 end_of_line integer
  let nl = string "\n\n"

  let input = lift3 (fun r _ l -> r, l) ranges nl ints
end

module Solving = struct

  let is_fresh ranges id =
    List.exists (fun (min, max) -> id >= min && id <= max) ranges

  let fusion (min1, max1) (min2, max2) =
    if max1 < min2 || max2 < min1
    then None
    else Some (min min1 min2, max max1 max2)

  let reduce_sorted ranges =
    match ranges with
    | [ ] -> [ ]
    | r1 :: rest ->
      let last, acc =
        List.fold_left (fun (prev, acc) r ->
            match fusion prev r with
            | None -> (r, prev :: acc)
            | Some r' -> (r', acc)
          ) (r1, []) rest
      in
      last :: acc

  let reduce ranges =
    List.sort compare ranges |> reduce_sorted

  let count ranges =
    List.fold_left (fun acc (min, max) -> acc + (max - min + 1)) 0 ranges

  let part1 (input : Types.input) : Types.output =
    Base.List.count ~f:(is_fresh (fst input)) (snd input)

  let part2 (input : Types.input) : Types.output =
    reduce (fst input) |> count
end

module Today = MakeDay (Types) (Parsing) (Solving)

let () = Today.run_all ()
