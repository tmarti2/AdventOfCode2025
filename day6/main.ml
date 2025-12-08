open Day
[@@@warning "-32"]
module Types = struct
  type op = Add | Mul [@@deriving show]
  let pp_array = Array_utils.pp_array
  let pp_matrix = Array_utils.pp_matrix Format.pp_print_int

  type matrix = int array array
  type input = matrix * op array [@@deriving show]
  type output = int [@@deriving show]
end

include Types

module Parsing = struct
  open Angstrom
  open Parsing

  let spaces = many space
  let numbers = sep_by1 spaces integer >>| Array.of_list
  let numbers =  enclosed spaces numbers
  let numbers_lines = sep_by end_of_line numbers >>| Array.of_list

  let add = char '+' *> return Add
  let mul = char '*' *> return Mul
  let ops = (sep_by1 spaces (add <|> mul)) <* spaces >>| Array.of_list
  let ops = enclosed spaces ops

  let input = lift3 (fun nbs _ ops -> nbs, ops) numbers_lines end_of_line ops
end

module Solving = struct

  open Array_utils

  let get_op a y =
    assert (valid a y);
    match a.(y) with
    | Add -> ( + )
    | Mul -> ( * )

  let compute_col y (nbs, ops) =
    let values = get_col_map nbs y in
    let op = get_op ops y in
    Base.List.reduce_exn ~f:op values

  let part1 (input : input) : output =
    let init = List.init (Array.length (snd input)) Fun.id in
    List.map (fun i -> compute_col i input) init
    |> Base.List.reduce_exn ~f:( + )

  let part2 (_input : input) : output = 0
end

module Today = MakeDay (Types) (Parsing) (Solving)

let () = Today.run_all ()
