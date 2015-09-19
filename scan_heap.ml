open Printf

type closure_dump = int * int * int array * int array

external closures : unit -> closure_dump = "dump_closure_addresses"

let closures () =
  Gc.full_major ();
  closures ()

let count_closures a =
  let h = Hashtbl.create 13 in
    Array.iter
      (fun addr ->
         try
           incr (Hashtbl.find h addr)
         with Not_found ->
           Hashtbl.add h addr (ref 1))
      a;
    Hashtbl.fold (fun addr c l -> (addr, !c) :: l) h [] |>
    List.sort compare

let addr_of_delta = function | `A (x, _) | `D (x, _) | `M (x, _) -> x
let count_of_delta = function | `A (_, x) | `D (_, x) | `M (_, x) -> abs x

let closure_delta (_, _, v1, v1') (_, _, v2, v2') =

  let rec step changes l1 l2 = match (l1, l2) with
    | [], [] -> changes
    | [], x :: tl -> step (`A x :: changes) l1 tl
    | x :: tl, [] -> step (`D x :: changes) tl l2
    | ((a1, c1) :: tl1), ((a2, c2) :: tl2) when a1 = a2 && c1 = c2 ->
        step changes tl1 tl2
    | ((a1, c1) :: tl1), ((a2, c2) :: tl2) when a1 = a2 ->
        step (`M (a1, c2 - c1) :: changes) tl1 tl2
    | (((a1, _) as x) :: tl1), ((a2, _) :: tl2) when a1 < a2 ->
        step (`D x :: changes) tl1 l2
    | _, (x :: tl2) (* when a1 > a2 *) ->
        step (`A x :: changes) l1 tl2 in

  let diff_vectors v1 v2 =
    let c1 = count_closures v1 in
    let c2 = count_closures v2 in
      step [] c1 c2 |>
      List.sort (fun d1 d2 -> count_of_delta d2 - count_of_delta d1)
  in
    (diff_vectors v1 v2, diff_vectors v1' v2')

let string_of_closure_delta = function
  | `A (a, c) -> sprintf "%x %d" a c
  | `D (a, c) -> sprintf "%x -%d" a c
  | `M (a, c) -> sprintf "%x %d" a c

let string_of_delta ((c1, b1, _, _) as x) ((c2, b2, _, _) as y) =
  let d1, d2 = closure_delta x y in
    sprintf
      "blocks: %d\n\
       bytes:  %d\n\
       == closures:\n\
       %s\n\
       == closure args:\n\
       %s\n
      "
      (c2 - c1)
      (b2 - b1)
      (String.concat "\n" @@ List.map string_of_closure_delta @@ d1)
      (String.concat "\n" @@ List.map string_of_closure_delta @@ d2)

let report_leaks ?closures:(dump_closures=false) f =
  let d0 = closures () in
  let s0 = Gc.full_major (); Objsize.objsize_roots () in
  lwt y  = f () in
  let () = Gc.full_major () in
  let s1 = Objsize.objsize_roots () in
  let d1 = closures () in
    printf "XXX DELTA: %s\n%!" Objsize.(show_info @@ sub s1 s0);
    List.iter print_endline @@ Objsize.(show_tags @@ sub s1 s0);
    if dump_closures then printf "XXX CLOSURES:\n%s\n%!" @@ string_of_delta d0 d1;
    Lwt.return y
