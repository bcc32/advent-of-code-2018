open! Core
open! Async
open! Import

module Line_spec = struct
  module One = struct
    type t =
      { lo : int
      ; hi : int
      }
  end

  type t =
    { x : One.t
    ; y : One.t
    }
end

let parse_line =
  let rex =
    let open Re in
    let f a b =
      seq
        [ char a
        ; char '='
        ; group (rep1 digit)
        ; str ", "
        ; char b
        ; char '='
        ; group (rep1 digit)
        ; str ".."
        ; group (rep1 digit)
        ]
    in
    compile (alt [ group (f 'x' 'y'); group (f 'y' 'x') ])
  in
  let parse_line s : Line_spec.t =
    let atoi = Int.of_string in
    let g = Re.exec rex s in
    if Re.Group.test g 1
    then (
      let x = Re.Group.get g 2 |> atoi in
      let y_lo = Re.Group.get g 3 |> atoi in
      let y_hi = Re.Group.get g 4 |> atoi in
      { x = { lo = x; hi = x }; y = { lo = y_lo; hi = y_hi } })
    else (
      let x_lo = Re.Group.get g 7 |> atoi in
      let x_hi = Re.Group.get g 8 |> atoi in
      let y = Re.Group.get g 6 |> atoi in
      { x = { lo = x_lo; hi = x_hi }; y = { lo = y; hi = y } })
  in
  parse_line
;;

module Point = struct
  type t =
    { x : int
    ; y : int
    }
  [@@deriving compare, hash, sexp_of]
end

module Material = struct
  type t =
    | Sand
    | Clay
    | Water_passed_through
    | Water_settled
  [@@deriving sexp_of]
end

let render (veins : Line_spec.t list) =
  let grid = Hashtbl.create (module Point) in
  veins
  |> List.iter ~f:(fun { x; y } ->
    for x = x.lo to x.hi do
      for y = y.lo to y.hi do
        Hashtbl.set grid ~key:{ x; y } ~data:Material.Clay
      done
    done);
  grid
;;

let main () =
  let%bind veins = Reader.file_lines "input" >>| List.map ~f:parse_line in
  let _grid = render veins in
  printf "output\n";
  return ()
;;

let%expect_test "a" =
  let%bind () = main () in
  [%expect {| output |}]
;;
