open Day

module Types = struct

  let pp_bool fmt b =
    let c = if b then '@'  else '.' in
    Format.fprintf fmt "%c" c

  let pp_input = Array_utils.pp_matrix pp_bool
  type input = bool array array
  type output = int [@@deriving show]
end

module Parsing = struct
  open Angstrom

  let paper = char '@' >>| fun _ -> true
  let empty = char '.' >>| fun _ -> false
  let line  = many1 (paper <|> empty) >>| Array.of_list
  let input = sep_by end_of_line line >>| Array.of_list
end

module Solving = struct

  let is_available a p =
    (Array_utils.count_neightbor_map ~f:Fun.id a p) < 4

  let remove_availables a l =
    List.iter (fun (x, y) -> a.(y).(x) <- false) l

  let find_availables a =
    let f y x (cells, nbcells) cell =
      if cell && is_available a (x, y) then
        (x, y) :: cells, nbcells + 1
      else (cells, nbcells)
    in
    Array_utils.foldi_map ~f ~init:([],0) a

  let find_all_availables a =
    let rec aux acc =
      let rm, n = find_availables a in
      if n = 0 then acc
      else (remove_availables a rm; aux (acc + n))
    in
    aux 0

  let part1 (input : Types.input) : Types.output =
    find_availables input |> snd

  let part2 (input : Types.input) : Types.output =
    find_all_availables input
end

module Today = MakeDay (Types) (Parsing) (Solving)

let () = Today.run_all ()
