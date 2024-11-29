(* Exercises from official website: https://ocaml.org/exercises *)

(* Beginner *)
let rec last = function
  | [] -> None
  | [x] -> Some x
  | _ :: t -> last t

let rec last_two = function
  | [] | [_] -> None
  | [x; y] -> Some (x, y)
  | _ :: t -> last_two t

exception IndexOutOfBounds of string

let rec nth n list =
if n < 0 then raise (IndexOutOfBounds "Index is out of bounds!")
else match list with 
  | [] -> raise (IndexOutOfBounds "Index is out of bounds!")
  | h :: t -> if n = 0 then Some h else nth (n - 1) t

let length list = 
  let rec length n = function
    | [] -> n
    | _ :: t -> length (n + 1) t
in 
length 0 list

let rev list = 
  let rec rev result = function
  | [] -> result
  | h :: t -> rev (h :: result) t
in 
rev [] list

let is_palindrome list = 
  if rev list = list then true else false

let encode list = 
  let rec encode count result = function
    | [] -> []
    | [x] -> (count + 1, x) :: result
    | h1 :: (h2 :: _ as t) -> 
      if h1 = h2 then 
        encode (count + 1) result t
      else
        encode 0 ((count + 1, h1) :: result) t
in 
rev (encode 0 [] list)

type 'a rle =
  | One of 'a
  | Many of int * 'a

let encode_modified list = 
  let constructor count value = 
    if count = 1 then One value else Many (count, value)
  in
  let rec encode count result = function
    | [] -> []
    | [x] -> constructor 1 x :: result
    | h1 :: (h2 :: _ as t) -> 
      if h1 = h2 then 
        encode (count + 1) result t
      else
        encode 0 (constructor (count + 1) h1 :: result) t
in 
rev (encode 0 [] list)

let rec duplicate = function
  | [] -> []
  | h :: t -> h :: h :: duplicate t

let split list first_part_length = 
  let rec split result first_part_length = function
    | [] -> List.rev result, []
    | h :: t as l -> if first_part_length = 0 then List.rev result, l else split (h :: result) (first_part_length - 1) t 
  in split [] first_part_length list;;


