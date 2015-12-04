(* Justin Raymond *)

let _ = Random.init

module Int = struct
  type t = int
  let compare x y = if x < y then -1 else if x = y then 0 else 1
end

module IH = Heap.Make(Int)

let random_list b n =
  let rec go n l =
    match n with
    | 0 -> l
    | _ -> go (n - 1) (Random.int b :: l)
  in go n []


let test = 
  let l = random_list 1000000 1000000 in
  let () = Printf.printf "%i\n" (List.length l); flush stdout in
  let ls = List.sort Int.compare l in
  let () = Printf.printf "list sorted\n"; flush stdout in
  let hs = IH.list_of_heap (IH.heap_of_list l) in
  let () = Printf.printf "heapsort\n"; flush stdout  in
  Printf.printf "%b\n" (ls = hs)
