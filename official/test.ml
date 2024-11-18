open Exercises

(* Test cases *)
let test_last_1 () =
  match last ["a"; "b"; "c"; "d"] with
  | Some value -> Alcotest.(check string) "Tail of a List" "d" value
  | None -> Alcotest.fail "Expected Some value but got None"
;;

let test_last_2 () =
  match last [] with
  | Some _ -> Alcotest.fail "Expected None but got Some value"
  | None -> ()
;;

let test_last_two_1 () =
  match last_two ["a"; "b"; "c"; "d"] with
  | Some (value1, value2) -> Alcotest.(check (pair string string)) "Last Two Elements of a List" ("c", "d") (value1, value2)
  | None -> Alcotest.fail "Expected Some value but got None"
;;

let test_last_two_2 () =
  match last_two ["a"] with
  | Some (_, _) -> Alcotest.fail "Expected None but got Some value"
  | None -> ()
;;

(* Run it *)
let () =
  let open Alcotest in
  run "Utils" [
    "last_1", [ test_case "Tail of a List" `Quick test_last_1 ];
    "last_2", [ test_case "Tail of an empty list" `Quick test_last_2 ];
    "last_two_1", [ test_case "Last Two Elements of a List" `Quick test_last_two_1 ];
    "last_two_2", [ test_case "Last Two Elements of a 1-element list" `Quick test_last_two_2 ];
  ]
;;
