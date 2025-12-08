open Day

module Types = struct
  let pp_array = Array_utils.pp_array

  type input = (float * float * float) array [@@deriving show]
  type output = int [@@deriving show]
end

include Types

module Parsing = struct
  open Angstrom
  open Parsing

  let int = integer >>| Float.of_int
  let middle_int = char_enclosed ',' int ','

  let box = lift3 (fun x y z -> (x,y,z)) int middle_int int

  let input = sep_by1 end_of_line box >>| Array.of_list
end

module Solving = struct

  let square x = x ** 2.0

  let distance (x1, y1, z1) (x2, y2, z2) =
    let dx = x2 -. x1 in
    let dy = y2 -. y1 in
    let dz = z2 -. z1 in
    sqrt (square dx +. square dy +. square dz)

  (* Compute distances between all couple of points and return an ordered Queue *)
  let connections boxes =
    let len = Array.length boxes in
    let connections = Queue.create () in
    let acc = ref [] in
    for i = 0 to len - 1 do
      for j = i + 1 to len - 1 do
        acc := (distance boxes.(i) boxes.(j), i, j) :: !acc;
      done
    done;
    let sorted = List.sort (fun (d1, _, _) (d2, _, _) -> compare d1 d2) !acc in
    List.iter (fun (_, id1, id2) -> Queue.push (id1, id2) connections) sorted;
    connections

  module ISet = Set.Make(Int)

  (* Will try to perform [limit] connections between boxes.
     Stop when reaching limit, if there is no remaining connection to do, or
     if all boxes are joined together.
  *)
  let join ~limit ~state connections =
    let len = Array.length state in
    let rec aux n =
      if n = limit then state
      else
        match Queue.peek_opt connections with
        | None -> state
        | Some (id1, id2) ->
          let union = ISet.union state.(id1) state.(id2) in
          ISet.iter (fun id -> state.(id) <- union) union;
          if ISet.cardinal union = len then state
          else begin
            ignore(Queue.pop connections);
            aux (n + 1)
          end
    in
    aux 0

  let max ((m1, m2, m3) as max) m =
    if m >= m1 then (m, m1, m2)
    else if m >= m2 then (m1, m, m2)
    else if m > m3 then (m1, m2, m)
    else max

  let find_max3_cardinal arr =
    (* Using a super set to keep only uniq elements *)
    let module ISetSet = Set.Make(ISet) in
    let sets = Array.fold_left (fun acc s -> ISetSet.add s acc) ISetSet.empty arr in
    ISetSet.fold (fun s acc -> max acc (ISet.cardinal s)) sets (1, 1, 1)

  let part_aux ?limit input =
    let connections = connections input in
    let limit = Option.value ~default:(Queue.length connections) limit in
    let state = Array.mapi (fun i _ -> ISet.singleton i) input in
    let arr = join ~limit ~state connections in
    connections, arr

  let part1 (input : Types.input) : Types.output =
    (* Settings are different for example and real input... *)
    let limit = if Array.length input = 20 then 10 else 1000 in
    let _, arr = part_aux ~limit input in
    let (m1, m2, m3) = find_max3_cardinal arr in
    m1 * m2 * m3

  let part2 (input : Types.input) : Types.output =
    let connections, _ = part_aux input in
    let (id1, id2) = Queue.top connections in
    let (x1, _, _), (x2, _, _) = input.(id1), input.(id2) in
    Int.of_float (x1 *. x2)

end

module Today = MakeDay (Types) (Parsing) (Solving)

let () = Today.run_all ()
