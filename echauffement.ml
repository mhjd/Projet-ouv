open Int64;;

type entier_precis = int64 list;;

let rec ajout_fin l x = 
  match l with 
  | [] -> [x]
  | hd::tl -> hd::(ajout_fin tl x);;

let recup_tete l = 
  match l with
  | [] -> failwith "Liste vide !"
  | hd::tl -> hd;;

let list_decapit l = 
  match l with
  | [] -> []
  | hd::tl -> tl;;

let to_bool (x:int64):bool = match x with zero -> false | _ -> true;;

let rec decompose_entier64 (hd : int64) : bool list = 
  match hd with 
  | 0L -> [false]
  | 1L -> [true]
  | _ -> let bit = unsigned_rem hd 2L and suite = unsigned_div hd 2L in (to_bool bit)::(decompose_entier64 suite);;

let rec decomposition (x : entier_precis) : bool list =
  match x with 
  | [] -> []
  | hd::tl -> (decompose_entier64 hd)@(decomposition tl)

