open Exercises

(* Test cases *)
let test_last_1 () =
  match last ["a"; "b"; "c"; "d"] with
  | Some value -> Alcotest.(check string) "Tail of a List" "d" value
  | None -> Alcotest.fail "Expected Some value but got None"

let test_last_2 () =
  match last [] with
  | Some _ -> Alcotest.fail "Expected None but got Some value"
  | None -> ()

let test_last_two_1 () =
  match last_two ["a"; "b"; "c"; "d"] with
  | Some (value1, value2) -> Alcotest.(check (pair string string)) "Last Two Elements of a List" ("c", "d") (value1, value2)
  | None -> Alcotest.fail "Expected Some value but got None"

let test_last_two_2 () =
  match last_two ["a"] with
  | Some (_, _) -> Alcotest.fail "Expected None but got Some value"
  | None -> ()

let test_nth_1 () =
  match nth 2 ["a"; "b"; "c"; "d"; "e"] with
  | Some value -> Alcotest.(check string) "Nth element of a list" "c" value
  | None -> Alcotest.fail "Expected Some value but got None"

let test_nth_2 () =
  try
    match nth 2 ["a"] with
    | Some _ | None -> Alcotest.fail "Expected exception but got None or Some value"
  with
  | IndexOutOfBounds _ -> ()
  | exn -> Alcotest.fail ("Unexpected exception: " ^ Printexc.to_string exn)

let test_length_1 () =
  match length ["a"; "b"; "c"] with
  | value -> Alcotest.(check int) "Length of a List" 3 value

let test_length_2 () =
  match length [] with
  | value -> Alcotest.(check int) "Length of a List" 0 value
  

let test_reverse () =
  let module M = Alcotest in
  M.(check (list string)) "Reverse a List" ["c"; "b"; "a"] (rev ["a"; "b"; "c"])

(* Run it *)
let () =
  let open Alcotest in
  run "Utils" [
    "last_1", [ test_case "Tail of a List" `Quick test_last_1 ];
    "last_2", [ test_case "Tail of an empty list" `Quick test_last_2 ];
    "last_two_1", [ test_case "Last Two Elements of a List" `Quick test_last_two_1 ];
    "last_two_2", [ test_case "Last Two Elements of a 1-element list" `Quick test_last_two_2 ];
    "nth_1", [ test_case "Find the N'th element of a list" `Quick test_nth_1 ];
    "nth_2", [ test_case "N'th element for index is out of bounds" `Quick test_nth_2 ];
    "length_1", [ test_case "Length of a List" `Quick test_length_1 ];
    "length_2", [ test_case "Length of a List" `Quick test_length_2 ];
    "reverse", [ test_case "Reverse a List" `Quick test_reverse ];
  ]
