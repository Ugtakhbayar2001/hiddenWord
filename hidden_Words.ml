open SetSig
open Hidden_Words_Sig
open Util
open UtilM


(* Here, we choose to not specify a "implementation" signature like
   `BSTreeImplS` one defined in `BSTree.ml`. Instead we leave the
   result of the functor to be "open" - that is, not sealed by any
   signature.
*)
module HiddenWordsImplF (S: SetS)  = struct

  let rec try_2_words_helper (w5: (char list * char list) list) (w3: string) (words8:string S.t) : (string * string * string) list =
    match w5 with
    | [] -> []
    | (pre, suf) :: xs when (not (pre = [] || suf = [])) -> 
        let word = implode (pre @ explode(w3) @ suf) in 
        if (S.elem word words8) then 
        (w3, implode (pre @ suf), implode (pre @ explode (w3) @ suf)) :: try_2_words_helper xs w3 words8
        else try_2_words_helper xs w3 words8
    | (pre, suf) :: xs -> try_2_words_helper xs w3 words8

  let rec try_2_words (words8: string S.t) (w3: string) (w5: string) : (string * string * string) list =
    let word = explode w5 in
    let w5_parts = all_parts word in
    try_2_words_helper w5_parts w3 words8

  let rec check_w3s (w3s: string list) (w5: string) (w8s: string S.t) : (string * string * string) list =
    match w3s with
    | [] -> []
    | x::xs -> (try_2_words w8s x w5) @ (check_w3s xs w5 w8s)

  let rec check_w5s (w3s: string list) (w5s: string list) (w8s: string S.t) : (string * string * string) list =
    match w5s with
    | [] -> []
    | x::xs -> (check_w3s w3s x w8s) @ (check_w5s w3s xs w8s) 

  let hidden_words (word_list: string list) : (string * string * string) list =
    let w3 = List.filter (fun n -> String.length n = 3) word_list in
    let w5 = List.filter (fun n -> String.length n = 5) word_list in
    let w8 = List.fold_left (fun set s -> S.insert s set ) S.empty (List.filter (fun n -> String.length n = 8) word_list) in
    check_w5s w3 w5 w8
  


end


(* Here we dfined the functor `HiddenF`. It uses the `HiddenImplF`
   functor but only exposes the `hidden_words` function that is
   defined in `HiddenS`.

   We will use `HiddenF` in `solution.ml` to create the list-based
   and the tree-based solutions to the problem.
 *)

module HiddenWordsF (S: SetS) : HiddenS = HiddenWordsImplF(S)
