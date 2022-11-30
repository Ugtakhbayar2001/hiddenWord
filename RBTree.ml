
(* Below is a signature for red-black trees that exposes some
   implementation details to facilitate testing.
 *)

module type  RBTreeImplS = sig

  type color = R | B

  type 'a tree = E
               | T of color * 'a tree * 'a * 'a tree

  include RBTreeSig.RBTreeS with type 'a t = 'a tree

  (* You may add additiona functions below that you wish to
     expose so that they can be more easily tested.

     Some implementations of `is_red_black_tree` may use helper
     function and you may add them here to facilitate testing.
   *)

end

module RBTreeImplM : RBTreeImplS = struct

  type color = R | B

  type 'a tree = E
               | T of color * 'a tree * 'a * 'a tree

  type 'a t = 'a tree

  let empty = E

  (* Lecture slides *)
  let balance c t1 v t2 = 
    match c , t1 , v , t2 with
    | B , T (R , T (R , a , x , b ) , y , c ) , z , d     (* left pattern *)
    | B , T (R , a , x , T (R , b  ,y , c )) , z , d      (* top patter *)
    | B , a , x , T (R , b , y , T (R , c , z , d))         (* right pattern *)
    | B , a , x , T (R , T(R , b , y , c) , z , d)        (* bottom pattern *)                                 
          -> T (R , T (B , a , x , b ) , y , T (B , c , z , d ))
    | c , t1 , v , t2 -> T ( c , t1 , v , t2 ) (* changed = to -> *)
    
    
  (* Lecture slides *)
  let rec insert ( x : 'a ) ( t : 'a t ) : 'a t =
    let rec ins (t': 'a t ) : 'a t =
    match t' with
    | E -> T (R , E , x , E )
    | T (c , t1 , y , t2 ) ->
        if x < y
        then balance c ( ins t1 ) y t2
        else if x > y
        then balance c t1 y ( ins t2 )
        else t'
    in
    match ins t with
    | E -> failwith " cannot happen , ins always returns a T t "
    | T (_ , t1 , y , t2 ) -> T (B , t1 , y , t2 )

  let rec elem (created: 'a) (t: 'a t) : bool =
    match t with 
    | E -> false
    | T (_, t1, v, t2) -> if v = created then true   
                          else if created < v then elem created t1
                          else elem created t2 
   
  let rec height (t: 'a t) : int =
    match t with
    | E -> 0
    | T (_, t1, v, t2) -> 1 + max (height t1) (height t2)

  let rec size (t: 'a t) : int =
    match t with
    | E -> 0
    | T (_, t1, v, t2) -> size t1 + 1 + size t2
      
  let rec minimum (t: 'a t): 'a option =
    let min_o (curr: 'a option) (v: 'a option) : 'a option =
      match curr, v with
      | None, None -> None
      | None, Some v -> Some v
      | Some v, None -> Some v 
      | Some v, Some v1 -> Some (min v v1)
    in
    match t with
    | E -> None
    | T (_, t1, v, t2) -> (min_o (min_o (Some v) (minimum t1)) (minimum t2))

  let o_greater (o1: 'a option) (o2: 'a option) : bool =
    match o1, o2 with 
    | None, Some i -> true
    | Some i, Some j -> i > j
    | _ -> true   

  let rec is_bst (t: 'a t) : bool = 
    let rec maximum (t: 'a t): 'a option =
      match t with
      | E -> None
      | T (_, t1, v, t2) -> (max (max (Some v) (maximum t1)) (maximum t2))
    in
    match t with
    | E -> true
    | T (_, E, v, E) -> true
    | T (_, t1, v, t2) -> 
        is_bst t1 && is_bst t2 && maximum t1 <= (Some v) && o_greater (minimum t2)  (Some v)    
  
  let is_b_root (t: 'a t) : bool =
    match t with
    | E -> true
    | T (B, t1, _, t2) -> true
    | _ -> false

  let rec no_rr (t: 'a t) : bool =
    match t with
    | E -> true
    | T (B, t1, _, t2) -> (no_rr t1) && (no_rr t2)
    | T (R, t1, _, t2) -> 
      (is_b_root t1) && (is_b_root t2) && (no_rr t1) && (no_rr t2)

  let rec b_count (t: 'a t) : int list =
    match t with
    | E -> [1]
    | T (B, t1, _, t2) -> 
      List.map (fun x -> x + 1) ((b_count t1) @ (b_count t2))
    | T (R, t1, _, t2) -> (b_count t1) @ (b_count t2)
  
  let rec eq_helper (ns: 'a list) ((so_far, top_element): bool * int) : bool =
    match ns with
    | [] -> so_far
    | x::xs -> eq_helper xs (x = top_element && so_far, x)
  
  let equal (ns: 'a list) : bool =
    match ns with
    | [] -> true
    | x :: xs -> eq_helper xs (true, x)

  let eq_blacks (t: 'a t) : bool =
    equal (b_count t)


  let is_red_black_tree (t: 'a t) : bool =
    (is_bst t) && (no_rr t) && (eq_blacks t) && (is_b_root t)

end



(* Below we create a new modules `RBTreeM` that only exposes the
   red-black tree functionality in the `RBTreeS` signature.
   Functions that may be useful for testing, such as `all_paths`
   are not accessible in `RBTreeM`.

   We "seal" `RBTreeM` with the signature `RBTreeS` so that it only
   exposes the elements of `RBTreeS`.   
*)
module RBTreeM : RBTreeSig.RBTreeS = RBTreeImplM

(* Below we create a new module `TreeSetM` that only exposes the
   set functionality in the `SetS` signature. Functions like
   `height` and `size` that are accessible in `RBTreeM` are not
   accessible in `RBTreeSetM`.

   We "seal" `RBTreeSetM` with the signature `SetS` so that is
   only exposes the elements of `SetS`.
 *)
module RBTreeBSTM : BSTreeSig.BSTreeS = RBTreeImplM



(* Below we create a new module `RBTreeSetM` that only exposes the
   set functionality in the `SetSig` signature. Functions like
   `height` and `size` that are accessible in `RBTreeM` are not
   accessible in `RBTreeSetM`.

   We "seal" `RBTreeSetM` with the signature `SetS` so that is
   only exposes the elements of `SetS`.
 *)
module RBTreeSetM : SetSig.SetS = RBTreeImplM
