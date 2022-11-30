
(* Below is a signature for binary search trees that exposes some
   implementation details to facilite testing.
 *)

module type  BSTreeImplS = sig
  type 'a tree = Leaf
               | Fork of 'a tree * 'a * 'a tree

  include BSTreeSig.BSTreeS with type 'a t = 'a tree

  (* You may add additiona functions below that you wish to
     expose so that they can be more easily tested.

     Some implementations of `is_bst` may use a function 
     `min_tree` and thus add the following to this signature:

     val min_tree : 'a tree -> 'a option

     You may add what ever functions you like here. Then add
     tests for them in `BSTreeSet_Tests_Impl.ml`
   *)

end


module BSTreeImplM : BSTreeImplS = struct

  type 'a tree = Leaf
               | Fork of 'a tree * 'a * 'a tree

  type 'a t = 'a tree


  let empty = Leaf

  let rec insert (elm: 'a) (t: 'a tree) : 'a tree =
    match t with
    | Leaf -> Fork (Leaf, elm, Leaf)
    | Fork (left, v, right) -> if elm < v then Fork (insert elm left, v, right) 
                              else if elm > v then Fork (left, v, insert elm right)
                              else Fork (left, v, right)

  let rec elem (created: 'a) (t: 'a tree) : bool =
    match t with 
    | Leaf -> false
    | Fork (left, v, right) -> if v = created then true   
                               else if created < v then elem created left
                               else elem created right  
   
  let rec height (t: 'a tree) : int =
    match t with
    | Leaf -> 0
    | Fork (left, v, right) -> 1 + max (height left) (height right)

  let rec size (t: 'a tree) : int =
    match t with
    | Leaf -> 0
    | Fork (left, v, right) -> size left + 1 + size right
      
  let rec minimum (t: 'a tree): 'a option =
    let min_o (curr: 'a option) (v: 'a option) : 'a option =
      match curr, v with
      | None, None -> None
      | None, Some v -> Some v
      | Some v, None -> Some v 
      | Some v, Some v1 -> Some (min v v1)
    in
    match t with
    |Leaf -> None
    |Fork (left, v, right) -> (min_o (min_o (Some v) (minimum left)) (minimum right))

  let o_greater (o1: 'a option) (o2: 'a option) : bool =
    match o1, o2 with 
    | None, Some i -> true
    | Some i, Some j -> i > j
    | _ -> true   

  let rec is_bst (t: 'a tree) : bool = 
    let rec maximum (t: 'a tree): 'a option=
      match t with
      |Leaf -> None
      |Fork (left, v, right) -> (max (max (Some v) (maximum left)) (maximum right))
    in
    match t with
    | Leaf -> true
    | Fork(Leaf, v, Leaf) -> true
    | Fork (left, v, right) -> 
        is_bst left && is_bst right && maximum left <= (Some v) && o_greater (minimum right)  (Some v)
        

end



(* Below we create a new moduled `BSTreeM` that only exposes the
   binary-search tree functionality in the BSTreeS` signature.
   Functions that may be useful for testing, such as `tree_min`
   are not accessible in `BSTreeM`.

   We "seal" `BSTreeM` with the signature `BSTreeS` so that it only
   exposes the elements of `BSTreeS`.
 *)
module BSTreeM : BSTreeSig.BSTreeS = BSTreeImplM



(* Below we create a new module `TreeSetM` that only exposes the
   set functionality in the `SetS` signature. Functions like
   `height` and `size` that are accessible in `TreeM` are not
   accessible in `TreeSetM`.

   We "seal" `TreeSetM` with the signature `SetS` so that is
   only exposes the elements of `SetS`.
 *)
module BSTreeSetM : SetSig.SetS = BSTreeM


