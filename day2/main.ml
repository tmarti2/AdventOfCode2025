open Day

module Types = struct
  type input = (int * int) list [@@deriving show]
  type output = int [@@deriving show]
end

module Parsing = struct
  open Angstrom
  open Parsing

  let range = lift3 (fun min _ max -> min,max) integer (char '-') integer
  let input = sep_by (char ',') range

end

module Solving = struct

  let is_invalid_p1 s =
    let len = String.length s in
    if len = 0 then true
    else if len mod 2 = 1 then false
    else
      let half = len / 2 in
      let first_half = String.sub s 0 half in
      let second_half = String.sub s half half in
      String.equal first_half second_half

  let is_invalid_p2 s =
    let len = String.length s in
    let rec check_pattern size =
      if size > len / 2 then false
      else if len mod size <> 0 then check_pattern (size + 1)
      else
        let rep = len / size in
        let pattern = String.sub s 0 size in
        let expected_string = String.concat "" (List.init rep (fun _ -> pattern)) in
        if String.equal s expected_string then true
        else check_pattern (size + 1)
    in
    check_pattern 1

  let part_aux ~f input =
    let f acc i = if f (string_of_int i) then (acc + i) else acc in
    List.fold_left (fun acc (min, max) ->
        Misc_utils.fold_range ~init:acc ~f ~min ~max
      ) 0 input

  let part1 (input : Types.input) : Types.output =
    part_aux ~f:is_invalid_p1 input

  let part2 (input : Types.input) : Types.output =
    part_aux ~f:is_invalid_p2 input
end

module Today = MakeDay (Types) (Parsing) (Solving)

let () = Today.run_all ()
