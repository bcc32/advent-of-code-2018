open! Core
open! Async
open! Import

module Square = struct
  type t =
    | Open
    | Wall
    | Unit of Game_unit.t
end

exception End_of_combat

type t = { grid : Square.t array array }

let create lines =
  let create_unit ~loc ~team =
    Game_unit.create ~loc ~hit_points:200 ~attack_power:3 ~team
  in
  let grid =
    Array.of_list_mapi lines ~f:(fun x row ->
        String.to_array row
        |> Array.mapi ~f:(fun y ->
             function
             | '.' -> Square.Open
             | '#' -> Wall
             | 'E' -> Unit (create_unit ~loc:{ x; y } ~team:Elf)
             | 'G' -> Unit (create_unit ~loc:{ x; y } ~team:Goblin)
             | char -> raise_s [%message "Unrecognized grid char" ~_:(char : char)]))
  in
  { grid }
;;

let get t ~x ~y = t.grid.(x).(y)

let units t =
  Array.to_list t.grid
  |> List.concat_map ~f:(fun row ->
         Array.to_list row
         |> List.filter_map ~f:(function
                | Square.Open -> None
                | Wall -> None
                | Unit u -> Some u))
;;

let perform_round t =
  let units =
    units t |> List.sort ~compare:(Comparable.lift [%compare: Loc.t] ~f:Game_unit.loc)
  in
  List.iter units ~f:(fun unit ->
  )
;;

let sum_of_hit_points t = assert false
