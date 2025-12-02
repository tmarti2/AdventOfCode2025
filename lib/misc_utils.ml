let ( << ) f g x = f (g x)

let int_list_to_int l =
  List.map string_of_int l |> String.concat "" |> int_of_string

let rec gcd a b = if b = 0 then a else gcd b (a mod b)

let lcm a b = (a * b) / gcd a b

let end_with c s =
  let last = String.get s (String.length s - 1) in
  if Char.equal last c then true else false