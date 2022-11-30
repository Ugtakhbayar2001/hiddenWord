open SetSig

module ListSetM : SetS = struct

  type 'a t = 'a list 
    
  let empty = []

  let insert (wrd: 'a) (lst: 'a list) : ('a list) =
    wrd :: lst

  let rec elem (created: 'a) (ans: 'a list) : bool =
    match ans with
    | []-> false
    | x::xs -> if x = created then true else elem created xs

end

