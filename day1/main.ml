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
  let line = left <|> right

  let input = sep_by end_of_line line


  type input = int * int


  let is_digit = Base.Char.is_digit
  let integer = take_while1 is_digit >>| int_of_string

end

module Solving = struct
  open Base

  let max = 100

  let value = function
    | L i -> -i
    | R i -> i

  let add dial v = (dial + v) % max

  let count_0_end dial r cpt =
    let res = add dial (value r) in
    if res = 0 then res, cpt + 1 else res, cpt

  let init = 50, 0

  let part1 (input : input) : output =
    List.fold_left ~init ~f:(fun (dial, cpt) r ->
        count_0_end dial r cpt
      ) input
    |> snd

  let part2 (_input : input) : output = 0
end

module Today = MakeDay (Types) (Parsing) (Solving)

let () = Today.run_all ()
