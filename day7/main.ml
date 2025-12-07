open Day

module Types = struct
  type cell = Start | Empty | Beam | Split

  let pp_tcell fmt (c, t) =
    match c with
    | Start -> Format.pp_print_char fmt 'S'
    | Empty -> Format.pp_print_char fmt '.'
    | Beam  -> Format.pp_print_int  fmt t
    | Split -> Format.pp_print_char fmt '^'

  let pp_input = Array_utils.pp_matrix ~sep:"" pp_tcell

  (* Cheating a bit, adding timeline for part 2 directly while parsing
     for convenience. *)
  type input = (cell * int) array array
  type output = int [@@deriving show]
end

include Types

module Parsing = struct
  open Angstrom

  let empty = char '.' *> return (Empty, 0)
  let start = char 'S' *> return (Start, 1)
  let split = char '^' *> return (Split, 0)

  let row = many1 (empty <|> start <|> split) >>| Array.of_list
  let input = sep_by1 end_of_line row >>| Array.of_list
end

module Solving = struct

  open Array_utils

  let splits = ref 0

  let propagate map t y x =
    let rec aux y x =
      match get_map map (x, y) with
      | None -> ()
      | Some (Empty, 0) -> map.(y).(x) <- Beam, t
      | Some (Beam, t') -> map.(y).(x) <- Beam, t + t'
      | Some (Split, _) ->
        incr splits;
        neightbors_cells map.(y) x |> List.iter (aux y)
      | _ -> assert false
    in
    aux y x

  let propagation map =
    iteri_map map ~f:(fun y x _ ->
        match get_map map (x, y) with
        | Some (Start, t) | Some (Beam, t)  ->
          propagate map t (y + 1) x
        | _ -> ()
      )

  let part1 (input : Types.input) : Types.output =
    splits := 0;
    propagation input;
    !splits

  let part2 (input : Types.input) : Types.output =
    (* No need to redo propagation, input is shared between part 1 and 2, and
       part 1 did the propagation already *)
    let last = input.(Array.length input - 1) in
    Array.fold_left (fun acc (c, t) ->
        match c with
        | Beam -> acc + t
        | _ -> acc
      ) 0 last

end

module Today = MakeDay (Types) (Parsing) (Solving)

let () = Today.run_all ()
