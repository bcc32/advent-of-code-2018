open! Core
open! Async
open! Import

module Square = struct
  type t =
    | Open
    | Wall
    | Unit of Game_unit.t
  [@@deriving sexp_of]
  let is_open = function Open -> true | Wall | Unit _ -> false
end

exception End_of_combat

type t = { grid : Square.t array array } [@@deriving sexp_of]

let to_string_hum t =
  Array.to_list t.grid
  |> List.map ~f:(fun row ->
    Array.to_list row
    |> List.map ~f:(function
      | Open -> '.'
      | Wall -> '#'
      | Unit u ->
        (match Game_unit.team u with
         | Elf -> 'E'
         | Goblin -> 'G'))
    |> String.of_char_list)
  |> String.concat ~sep:"\n"
;;

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

let get t { Loc.x; y } = t.grid.(x).(y)

let units t =
  Array.to_list t.grid
  |> List.concat_map ~f:(fun row ->
    Array.to_list row
    |> List.filter_map ~f:(function
      | Square.Open -> None
      | Wall -> None
      | Unit u -> Some u))
;;

let calculate_distances t ~start =
  let q = Queue.create () in
  Queue.enqueue q start;
  let distance = Loc.Table.create () in
  Hashtbl.add_exn distance ~key:start ~data:0;
  while not (Queue.is_empty q) do
    let loc = Queue.dequeue_exn q in
    Loc.adjacent loc ~height:(Array.length t.grid) ~width:(Array.length t.grid.(0))
    |> List.iter ~f:(fun loc' ->
      if Square.is_open (get t loc')
      then
        match Hashtbl.mem distance loc' with
        | true -> ()
        | false ->
          Hashtbl.add_exn distance ~key:loc' ~data:(Hashtbl.find_exn distance loc + 1);
          Queue.enqueue q loc')
  done;
  distance
;;

let try_perform_attack t ~unit =
  match
    Loc.adjacent
      (Game_unit.loc unit)
      ~height:(Array.length t.grid)
      ~width:(Array.length t.grid.(0))
    |> List.filter_map ~f:(fun loc ->
      match get t loc with
      | Open | Wall -> None
      | Unit unit' when not (Team.equal (Game_unit.team unit) (Game_unit.team unit'))
        -> Some (loc, unit')
      | Unit _ -> None)
    |> List.min_elt
         ~compare:
           (Comparable.lexicographic
              [ Comparable.lift [%compare: int] ~f:(fun (_loc, unit) ->
                  Game_unit.hit_points unit)
              ; Comparable.lift [%compare: Loc.t] ~f:fst
              ])
  with
  | None -> (* no targets, do nothing *) ()
  | Some (loc, unit') ->
    (match Game_unit.receive_damage unit' ~points:(Game_unit.attack_power unit) with
     | `Alive -> ()
     | `Dead -> t.grid.(loc.x).(loc.y) <- Open)
;;

let perform_round_for_unit t ~unit ~units =
  match
    List.filter units ~f:(fun u ->
      Game_unit.is_alive u &&
      not (Team.equal (Game_unit.team u) (Game_unit.team unit)))
  with
  | [] -> raise End_of_combat
  | _ :: _ as targets ->
    let open_squares =
      targets
      |> List.concat_map ~f:(fun target ->
        Loc.adjacent
          (Game_unit.loc target)
          ~height:(Array.length t.grid)
          ~width:(Array.length t.grid.(0)))
      |> List.filter ~f:(fun loc ->
        match get t loc with
        | Open -> true
        | Wall -> false
        | Unit u -> phys_equal u unit)
    in
    (match List.mem open_squares (Game_unit.loc unit) ~equal:[%equal: Loc.t] with
     | true -> try_perform_attack t ~unit
     (* already adjacent *)
     | false ->
       (* move towards an open square *)
       (match open_squares with
        | [] -> (* can't move, do nothing *) ()
        | _ :: _ ->
          let distance_to =
            let tbl = calculate_distances t ~start:(Game_unit.loc unit) in
            fun loc ->
              Option.value_map (Hashtbl.find tbl loc) ~f:float ~default:Float.infinity
          in
          let target_square =
            List.min_elt
              open_squares
              ~compare:
                (Comparable.lexicographic
                   [ Comparable.lift [%compare: float] ~f:distance_to; [%compare: Loc.t] ])
            |> Option.value_exn
          in
          let distance_to_target =
            let tbl = calculate_distances t ~start:target_square in
            fun loc ->
              Option.value_map (Hashtbl.find tbl loc) ~f:float ~default:Float.infinity
          in
          let step_square =
            Loc.adjacent
              (Game_unit.loc unit)
              ~height:(Array.length t.grid)
              ~width:(Array.length t.grid.(0))
            |> List.filter ~f:(fun loc ->
              match get t loc with
              | Open -> true
              | Wall | Unit _ -> false)
            |> List.min_elt
                 ~compare:
                   (Comparable.lexicographic
                      [ Comparable.lift [%compare: float] ~f:distance_to_target
                      ; [%compare: Loc.t]
                      ])
          in
          (match step_square with
           | None -> ()
           | Some step_square ->
             let old_square = Game_unit.loc unit in
             Game_unit.set_loc unit step_square;
             t.grid.(old_square.x).(old_square.y) <- Open;
             t.grid.(step_square.x).(step_square.y) <- Unit unit;
             try_perform_attack t ~unit)))
;;

let perform_round t =
  let units =
    units t |> List.sort ~compare:(Comparable.lift [%compare: Loc.t] ~f:Game_unit.loc)
  in
  List.iter units ~f:(fun unit ->
    if Game_unit.is_alive unit then perform_round_for_unit t ~unit ~units)
;;

let sum_of_hit_points t =
  units t
  |> List.filter ~f:Game_unit.is_alive
  |> List.sum (module Int) ~f:Game_unit.hit_points
;;
