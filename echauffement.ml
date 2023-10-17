type entier_precis Int64.Int64 list

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