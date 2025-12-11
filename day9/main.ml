open Day

module Types = struct
  let pp_array = Array_utils.pp_array
  type input = (int*int) array [@@deriving show]
  type output = int [@@deriving show]
end

include Types

module Parsing = struct
  open Angstrom
  open Parsing

  let point = lift3 (fun l _ r -> l,r) integer (char ',') integer
  let input = sep_by end_of_line point >>| Array.of_list
end

module Solving = struct

  let sort x y = if x <= y then x, y else y, x

  let rect_size ((x1,y1),(x2, y2)) =
    let minx, maxx = sort x1 x2 in
    let miny, maxy = sort y1 y2 in
    (maxx - minx + 1) * (maxy - miny + 1)

  let get_pairs rects =
    let len = Array.length rects in
    let acc = ref [] in
    for i = 0 to len - 2 do
      for j = i + 1 to len - 1 do
        acc := (rects.(i), rects.(j)) :: !acc;
      done
    done;
    !acc

  let find_max rects =
    let pairs = get_pairs rects in
    List.fold_left (fun acc r -> max acc (rect_size r)) 0 pairs

  let part1 (input : Types.input) : Types.output =
    find_max input

  let part2 (_input : Types.input) : Types.output = 0

end

module Today = MakeDay (Types) (Parsing) (Solving)

let () = Today.run_all ()
