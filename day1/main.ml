open Day

module Types = struct
  type rot = L of int | R of int [@@deriving show]
  type input = rot list [@@deriving show]
  type output = int [@@deriving show]
end

include Types

module Parsing = struct
  open Angstrom
  open Parsing

  let left  = char 'L' *> integer >>| fun i -> L i
  let right = char 'R' *> integer >>| fun i -> R i
  let line  = left <|> right

  let input = sep_by end_of_line line
end

module Solving = struct
  open Base

  let max = 100

  let init = 50, 0

  let count_0_end dial r cpt =
    let v = match r with L i -> -i | R i -> i in
    let res = (dial + v) % max in
    if res = 0 then res, cpt + 1 else res, cpt

  let count_0_pass dial rot cpt =
    match rot with
    | R i ->
      let raw = dial + i in
      raw % max, cpt + raw / max
    | L i ->
      let raw_dial = dial - i in
      let new_dial = raw_dial % max in
      let zeros =
        if dial = 0 then i / max
        else if dial = i then 1
        else if dial < i then (max - dial + i) / max
        else 0
      in
      (new_dial, cpt + zeros)

  let part_aux ~f input =
    List.fold_left ~init ~f:(fun (dial, cpt) r -> f dial r cpt) input
    |> snd

  let part1 (input : input) : output =
    part_aux ~f:count_0_end input

  let part2 (input : input) : output =
    part_aux ~f:count_0_pass input
end

module Today = MakeDay (Types) (Parsing) (Solving)

let () = Today.run_all ()
