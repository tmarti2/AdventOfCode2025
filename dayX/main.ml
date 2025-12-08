open Day

module Types = struct
  type input = int [@@deriving show]
  type output = int [@@deriving show]
end

include Types

module Parsing = struct
  open Angstrom
  open Parsing

  let input = integer >>| Fun.id
end

module Solving = struct

  let part1 (_input : Types.input) : Types.output = 0

  let part2 (_input : Types.input) : Types.output = 0

end

module Today = MakeDay (Types) (Parsing) (Solving)

let () = Today.run_all ()
