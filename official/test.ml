open Alcotest

(* The tests *)
let test_last () =
  Alcotest.(check string) "d" (Exercises.last ["a" ; "b" ; "c" ; "d"])
;;

(* Run it *)
let () =
  let open Alcotest in
  run "Utils" [
      "last", [ test_case "Tail of a List" `Quick test_last  ];
  ]
(* 
let test_last () =
  let input = [1; 2; 3; 4] in
  let expected = Some 4 in
  let result = last input in
  match result with
  | Some r when Some r = expected -> 
      Printf.printf "test_last PASSED\n"
  | Some r -> 
      Printf.printf "test_last FAILED: Expected %d, but got %d\n" (match expected with Some v -> v | None -> -1) r
  | None -> 
      Printf.printf "test_last FAILED: Expected Some %d, but got None\n" (match expected with Some v -> v | None -> -1)

let () = test_last () *)