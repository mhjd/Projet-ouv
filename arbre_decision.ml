open Grands_entiers

(*Question 7*)
(*Définition d'un type profondeur*)
type profondeur = int ;;

(*Structure de données servant à la compression*)
type arbre_decision =
  | Noeud of profondeur * arbre_decision  * arbre_decision 
  | Feuille of bool ;;

  (*Explication de la structure : 
    En OCaml, les types Sommes sont faits avec des pointeurs vers ses éléments (lorsque ce ne sont pas des entiers).
    Si on veut changer le fils d'un père, il suffit de lui donner à la construction un fils, ce qui sera équivalent 
    à rediriger un pointeur, dans un langage à la C par exemple.*)


(*Question 8*)
(*Fonction générant un arbre de décision associé à la table de vérité t *)
let cons_arbre (t : bool list) : arbre_decision =
  let rec cons_arbre_head (t : bool list) (dpth : int) : arbre_decision =
    match t with 
    | [] -> failwith "Cas impossible"
    | h1::h2::[] -> Noeud(dpth, ( (Feuille(h1))), ( (Feuille(h2))))
    | l -> let nb_feuilles = (List.length t) / 2 in
        let sous_arbre_droit = cons_arbre_head (completion l nb_feuilles) (dpth+1) in
        let sous_arbre_gauche = cons_arbre_head (suite_liste l nb_feuilles) (dpth+1) in
        Noeud(dpth,  sous_arbre_droit ,  sous_arbre_gauche)
  in cons_arbre_head t 1

  (* -- Test fait dans le TopLevel :
    cons_arbre (table 25899 16) ;;
  *) 

  
(*Question 9*)
(*Renvoie les feuilles d'un arbre ou un sous-arbre*)
let rec liste_feuille (arbre : arbre_decision): bool list =
  match arbre with
  | Noeud(dpth, sous_arbre_gauche, sous_arbre_droit) -> (liste_feuille ( sous_arbre_gauche)) @ (liste_feuille (sous_arbre_droit))
  | Feuille(my_bool) -> [my_bool]

(* -- Tests faits dans le TopLevel :
Pour un arbre : 
liste_feuille (cons_arbre (table 25899 16));;
Pour un sous-arbre :
let Noeud(_, sag, _) = cons_arbre (table 25899 16) in liste_feuille sag
*)
