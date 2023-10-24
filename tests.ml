open Compression 
open Arbre_decision
open Grands_entiers


(*On peut faire les tests ici*)

let rec print_entier_precis (l : entier_precis) : unit = 
  match l with
  | [] -> Printf.printf "\n"
  | hd::tl -> Printf.printf "%Ld \t" hd ; (print_entier_precis tl);;


let main  = 
  print_entier_precis (genAlea 42);;