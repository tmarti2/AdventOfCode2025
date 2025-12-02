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

  let invalid_pattern s len size =
    if len mod size <> 0 then false
    else
      let pattern = String.sub s 0 size in
      List.init ((len / size) -1) Fun.id
      |> List.for_all (fun i ->
          String.sub s ((i + 1) * size) size |> String.equal pattern
        )

  let is_invalid_p1 s =
    let len = String.length s in
    if len mod 2 = 1 then false
    else invalid_pattern s len (len / 2)

  let is_invalid_p2 s =
    let len = String.length s in
    let rec aux size =
      if size = 0 then false
      else if invalid_pattern s len size then true
      else aux (size - 1)
    in
    aux (len / 2)

  let part_aux ~domain_mgr ~f input =
    let f acc i = if f (string_of_int i) then (acc + i) else acc in
    Eio.Fiber.List.map (fun (min, max) ->
        Eio.Domain_manager.run domain_mgr @@ fun () ->
        Misc_utils.fold_range ~init:0 ~f ~min ~max
      ) input
    |> List.fold_left ( + ) 0

  let part_aux ~f input =
    Eio_main.run @@ fun env ->
    part_aux ~domain_mgr:(Eio.Stdenv.domain_mgr env) ~f input

  let part1 (input : Types.input) : Types.output =
    part_aux ~f:is_invalid_p1 input

  let part2 (input : Types.input) : Types.output =
    part_aux ~f:is_invalid_p2 input
end

module Today = MakeDay (Types) (Parsing) (Solving)

let () = Today.run_all ()
