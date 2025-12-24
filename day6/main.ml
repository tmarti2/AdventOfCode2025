open Day

[@@@warning "-32"]

module Types = struct
  type op = Add | Mul [@@deriving show]
  let pp_array = Array_utils.pp_array
  let pp_matrix = Array_utils.pp_matrix (Format.pp_print_option Format.pp_print_int)

  type mdr = int list [@@deriving show]

  type matrix = (int option) array array
  type input = matrix * op array [@@deriving show]
  type output = int [@@deriving show]
end

include Types

module Parsing = struct
  open Angstrom
  open Parsing

  let space = space >>| fun _ -> None
  let int   = digit >>| fun i -> Some i
  let number = space <|> int
  let number_line = many1 number >>| Array.of_list
  let number_lines = sep_by1 end_of_line number_line >>| Array.of_list

  let add = char '+' *> return Add
  let mul = char '*' *> return Mul
  let op = (add <|> mul) <* many1 space
  let ops = many1 op >>| Array.of_list

  let input = lift3 (fun nbs _ ops -> nbs, ops) number_lines end_of_line ops
end

module Solving = struct

  open Array_utils

  let get_op a y =
    assert (valid a y);
    match a.(y) with
    | Add -> ( + )
    | Mul -> ( * )

  let compute_aux ~f ~init ~check row =
    let cur, acc = Array.fold_left f init row in
    let acc = if check cur then acc else cur :: acc in
    List.rev acc |> Array.of_list

  let compute_row row =
    let f (cur, acc) = function
      | None when cur <> 0 -> (0, cur :: acc)
      | None -> (cur, acc)
      | Some x -> (cur * 10 + x, acc)
    in
    compute_aux ~f ~init:(0,[]) ~check:((=) 0) row

  let nbs_to_row nbs =
    Array.fold_left (fun acc row -> compute_row row :: acc) [] nbs
    |> List.rev |> Array.of_list

  let nbs_to_col nbs =
    let nbs = Base.Array.transpose_exn nbs in
    let nbs = nbs_to_row nbs in
    let f (cur, acc) = function
      | [| |] -> ([], cur :: acc)
      | [| x |] -> (x :: cur, acc)
      | _ -> assert false
    in
    compute_aux ~f ~init:([],[]) ~check:((=) []) nbs

  let compute_col ~get y nbs ops =
    let values = get nbs y in
    let op = get_op ops y in
    Base.List.reduce_exn ~f:op values

  let part_aux ~get ~conv nbs ops =
    let init = List.init (Array.length ops) Fun.id in
    let nbs = conv nbs in
    List.map (fun i -> compute_col ~get i nbs ops) init
    |> Base.List.reduce_exn ~f:( + )

  let part1 ((nbs, ops) : input) : output =
    part_aux ~get:get_col_map ~conv:nbs_to_row nbs ops

  let part2 ((nbs, ops) : input) : output =
    part_aux ~get:Array.get ~conv:nbs_to_col nbs ops
end

module Today = MakeDay (Types) (Parsing) (Solving)

let () = Today.run_all ()
