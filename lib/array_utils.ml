open Base
open Misc_utils

let pp_array pp_elem fmt s =
  let pp_sep ppf () = Stdlib.Format.fprintf ppf "|" in
  Stdlib.Array.to_seq s |> Stdlib.Format.pp_print_seq ~pp_sep pp_elem fmt

let pp_matrix pp_elem fmt s =
  let pp_sep ppf () = Stdlib.Format.fprintf ppf "\n" in
  Stdlib.Array.to_seq s
  |> Stdlib.Format.pp_print_seq ~pp_sep (pp_array pp_elem) fmt

let find_max_id a =
  assert (Array.length a > 0);
  Array.foldi a ~init:(a.(0), 0) ~f:(fun i (max, mid) x ->
      if x > max then (x, i) else (max, mid)
    )

let valid a x = x >= 0 && x < Array.length a
let valid_map map (x, y) = valid map y && valid map.(y) x
let get a x = if valid a x then Some a.(x) else None
let get_map map (x, y) = if valid_map map (x, y) then Some map.(y).(x) else None
let adj x = [ x - 1; x + 1 ]

let adj_map (x, y) =
  [
    x - 1, y - 1;
    x - 1, y;
    x - 1, y + 1;
    x, y - 1;
    x, y + 1;
    x + 1, y - 1;
    x + 1, y;
    x + 1, y + 1;
  ]

let neightbors_cells a p = List.filter (adj p) ~f:(valid a)
let neightbors_cells_map a p = List.filter (adj_map p) ~f:(valid_map a)
let neightbors a p = List.filter_map (adj p) ~f:(get a)
let neightbors_map a p = List.filter_map (adj_map p) ~f:(get_map a)

let filter_aux ~get ~adj ~f a p =
  let f = Option.value_map ~default:false ~f in
  List.filter ~f:(f << get a) (adj a p)

let filter_cells ~f a p = filter_aux ~get ~adj:neightbors_cells ~f a p

let filter_cell_map ~f a p =
  filter_aux ~get:get_map ~adj:neightbors_cells_map ~f a p

let filter_neightbors ~f a p = List.filter ~f (neightbors a p)
let filter_neightbors_map ~f a p = List.filter ~f (neightbors_map a p)

let exist_neightbor ~f a p = List.exists ~f (neightbors a p)
let exist_neightbor_map ~f a p = List.exists ~f (neightbors_map a p)

let count_neightbor ~f a p = Base.List.count ~f (neightbors a p)
let count_neightbor_map ~f a p = Base.List.count ~f (neightbors_map a p)

let foldi_map ~f ~init a =
  Base.Array.foldi ~init ~f:(fun y acc row ->
      Base.Array.foldi ~init:acc ~f:(f y) row
    ) a