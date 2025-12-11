open Day

module Types = struct
  type input = (string * string list) list [@@deriving show]
  type output = int [@@deriving show]
end

include Types

module Parsing = struct
  open Angstrom
  open Parsing

  let dst = sep_by1 space word

  let src = word <* string ": "

  let line = both src dst

  let input = sep_by1 end_of_line line
end

module Solving = struct

  let fill input =
    let graph = Hashtbl.create 17 in
    List.iter (fun (src, dsts) -> Hashtbl.add graph src dsts) input;
    graph

  (* Memo is global so it can be shared between part 1 and 2. *)
  let memo = Hashtbl.create 17

  let find_paths ~src ~dst g =
    let rec aux todo acc =
      match todo with
      | [] -> acc
      | node :: rest ->
        if node = dst then aux rest (acc + 1)
        else
          match Hashtbl.find_opt memo node with
          | Some v -> aux rest (acc + v)
          | None ->
            let n = Hashtbl.find_opt g node |> Option.value ~default:[] in
            let res = aux n 0 in
            Hashtbl.add memo node res;
            aux rest (acc + res)
    in
    aux [src] 0

  let part1 (input : Types.input) : Types.output =
    let src = "you" in
    let dst = "out" in
    let g = fill input in
    Hashtbl.clear memo;
    find_paths ~src ~dst g

  (* Avoid computation if the prev computed part of the path is impossible *)
  let find_path_part ~src ~dst g prev =
    if prev = 0 then prev else prev * find_paths ~src ~dst g

  let part2 (input : Types.input) : Types.output =
    let g = fill input in
    (* path1: svr -> dac -> fft -> out
       path2: svr -> fft -> dac -> out *)

    (* No need to reset after part 1, since we have the same destination *)
    let path1 = find_path_part ~src:"fft" ~dst:"out" g 1 in
    let path2 = find_path_part ~src:"dac" ~dst:"out" g 1 in

    Hashtbl.clear memo;

    let path1 = find_path_part ~src:"svr" ~dst:"dac" g path1 in
    let path2 = find_path_part ~src:"fft" ~dst:"dac" g path2 in

    Hashtbl.clear memo;

    let path1 = find_path_part ~src:"dac" ~dst:"fft" g path1 in
    let path2 = find_path_part ~src:"svr" ~dst:"fft" g path2 in

    path1 + path2

end

module Today = MakeDay (Types) (Parsing) (Solving)

let () = Today.run_all ()
