(* Justin Raymond 
 *
 * Implementation of a heap using a Pairing Heap *)

(* A pairing heap is either an empty heap or a Node of an element and
 * a list of pairing heaps.
 * INVARIANT: E never appears in the child list of a node.
 * INVARIANT: The value at a node is always less than or equal to its children.
 *
 * Worst case running times:
 *   find_min, insert, merge: O(1)
 *   delete_min: O(n)
 * Amortized running times:
 *   insert, merge, delete_min: O(logn)
 * Note:
 *   conjectured but not proved that insert and merge run in O(1) amortized time.
 *
 * I think you will find in practice that these heaps are quite fast ;)
 *
 * Source: Okasaki: Purely Functional Data Structures (1998)
 *)
open Ord

module Make (Ord : Orderable) = struct
  type elem = Ord.t
  let min x y = if Ord.compare x y <= 0 then x else y
  type heap = E | N of elem * heap list
  exception EMPTY

  (* The empty heap *)
  let empty = E

  (* O(1) *)
  let is_empty h = 
    match h with
    | E -> true
    | _ -> false

  (* merges two heaps. O(1) worst case *)
  let merge h1 h2 =
    match (h1,h2) with
    | (E, _) -> h2
    | (_, E) -> h1
    | (N (x, xs), N (y, ys)) when x <= y -> N (x, h2::xs)
    | (_, N (y, ys)) -> N (y, h1::ys)

  (* merges a list of heaps into one by making two passes.
   * The first merges heaps in pairs from left to right.
   * The second merges the resulting pairs into a single heap
   * from right to left. *)
  let merge_pairs hs =
    let rec merge_lr hs l =
      (match hs with
      | [] -> l
      | (x::[]) -> x::l
      | (x::y::xs) -> merge_lr xs (merge x y::l)) in
    let rec merge_rl hs h =
      (match hs with
      | [] -> h
      | x::xs -> merge_rl xs (merge h x)) in
    merge_rl (merge_lr hs []) E

  (* inserts element into heap. *)
  let insert h x = merge h (N (x, []))

  (* returns min element and tree with min element removed *)
  let pop_min h =
    match h with
    | N (x, xs) -> (x, merge_pairs xs)
    | _ -> raise EMPTY

  (* returns min element. *)
  let find_min h = 
    match h with
    | N (x, _) -> Some x
    | _ -> raise EMPTY

  (* removes min element and returns tree *)
  let delete_min h = 
    match h with
    | N (x, xs) -> merge_pairs xs
    | _ -> raise EMPTY

  let heap_of_list = List.fold_left insert empty

  let list_of_heap h = 
    let rec go h l =
      if is_empty h
      then l
      else
        let (x, h') = pop_min h
        in go h' (x::l)
    in List.rev (go h [])

  let heapsort l = list_of_heap (heap_of_list l)
end
