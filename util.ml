(* You are free to add additional functions here.

   But do not change the existing ones.
 *)

module UtilM = struct

  let implode (cs: char list) : string =
    String.concat "" (List.map  (String.make 1) cs)

  let explode (s: string) : char list =
    let len = String.length s in
    let rec f i = if i = len then [] else s.[i] :: f (i+1) in
    f 0

  let rec all_suffixes (w: 'a list) accumulator : 'a list list =
    match w with
    | [] -> accumulator
    | x::xs -> all_suffixes xs (accumulator @ [x::xs])

  let all_prefixes (w: 'a list) : 'a list list =
    let rec find_all_prefixes (lst: 'a list) (previous: 'a list) (prefix_list: 'a list list) : 'a list list =
      match lst with
      | [] -> prefix_list
      | x::[] -> ([previous @ [x]]) @ prefix_list
      | x::xs -> find_all_prefixes xs (previous @ [x]) ([previous @ [x]]) @ prefix_list
    in
    find_all_prefixes (w) [] [[]]
    
  let all_parts (w: 'a list) : ('a list * 'a list) list =
    let rec construct (wPre: 'a list list) (wSuf: 'a list list)  (all_suf_pref: ('a list * 'a list) list) =
      match wPre, wSuf with
      | [[]], [[]] -> all_suf_pref
      | ((_, [])) -> all_suf_pref
      | ([], _) -> [([], [])]
      | x::xs, y::ys -> if all_suf_pref = [([], [])] 
                        then construct xs ys [(x, y)] 
                        else construct xs ys ((x, y) :: all_suf_pref)
    in
    let prefixes = (List.rev(all_prefixes w)) 
    in 
    let suffixes = (all_suffixes w []) 
    in construct prefixes suffixes [([],[])]

  let read_words (file_name: string) : string list =
    let ic = open_in file_name in
    let rec read_lines ic = try
        let next_line = input_line ic in
        next_line :: read_lines ic
      with _ -> []
    in
    let raw_strings = read_lines ic 
    in
    List.filter (fun s -> String.length s > 0)
      (List.map String.trim raw_strings)

  let rec print_answers (ans: (string * string * string) list) =
    match ans with
    | [] -> ()
    | (w3,w5,w8) :: rest -> 
       print_endline ( "(" ^ w3 ^ ", " ^ w5 ^ ", " ^ w8 ^ ")" );
       print_answers rest

end
