open! Core
open! Async
open! Import

let main () =
  let%bind lines =
    Reader.with_file "input" ~f:(fun r -> r |> Reader.lines |> Pipe.to_list)
  in
  let battlefield = Battlefield.create lines in
  let full_rounds =
    let rec loop rounds =
      try
        Battlefield.perform_round battlefield;
        loop (rounds + 1)
      with
      | Battlefield.End_of_combat -> rounds
    in
    loop 0
  in
  let outcome = full_rounds * Battlefield.sum_of_hit_points battlefield in
  printf "%d\n" outcome;
  return ()
;;

let%expect_test "a" =
  let%bind () = show_raise' (fun () -> main ()) in
  [%expect {| output |}]
;;
