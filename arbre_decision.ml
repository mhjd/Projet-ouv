open Grands_entiers



(*Question 7*)
(*Définition d'un type profondeur*)
type profondeur = int ;;


(*Structure de données servant à la compression*)
type arbre_decision =
  | Noeud of profondeur * arbre_decision ref * arbre_decision ref
  | Feuille of bool ;;

  (*Explication de la structure : 
    Une référence est une adresse, c'est à dire un entier. Lorsqu'on reconstruit l'arbre en le parcourant, 
    avec nos appels récursifs, on récupère les valeurs des fils calculés et on les met dans la structure courante.
    Tout se passe par copie en OCaml, quand on va retourner, par exemple, au père, le fils gauche du nœud courant, 
    on lui retourne l'adresse vers ce fils (c'est une ref), donc on copie un entier, mais qui pointe vers la bonne
    représentation mémoire. Ainsi, on ne duplique pas les données, seulement les pointeurs vers les données.*)


(*Question 8*)
(*Fonction générant un arbre de décision associé à la table de vérité t
La fonction head permet d'éviter de devoir préciser une profondeur, devant toujours être égale à 1 lors de l'appel
Cela serait pertinent de créer un type table_de_verite, au lieu de directement utiliser "bool list", qui est pas très propre car on a d'autre liste de bool
*)
(*
compressionParListe( ref (cons_arbre (table 25899 16))) [];;
*)

let cons_arbre (t : bool list) : arbre_decision =
  let rec cons_arbre_head (t : bool list) (dpth : int) : arbre_decision =
    match t with 
    | [] -> failwith "Cas impossible"
    | h1::h2::[] -> Noeud(dpth, (ref (Feuille(h1))), (ref (Feuille(h2))))
    | l -> let nb_feuilles = (List.length t) / 2 in
        let sous_arbre_droit = cons_arbre_head (completion l nb_feuilles) (dpth+1) in
        let sous_arbre_gauche = cons_arbre_head (suite_liste l nb_feuilles) (dpth+1) in
        Noeud(dpth, ref sous_arbre_droit , ref sous_arbre_gauche)
  in cons_arbre_head t 1

(* Exemple de test, repris de l'énoncé :
cons_arbre (table 25899 16) ;;
*) 

(*Question 9*)
(*Renvoie les feuilles d'un arbre ou un sous-arbre*)
let rec liste_feuille (arbre : arbre_decision): bool list =
  match arbre with
  | Noeud(dpth, sous_arbre_gauche, sous_arbre_droit) -> (liste_feuille ( ! sous_arbre_gauche)) @ (liste_feuille (!sous_arbre_droit))
  | Feuille(my_bool) -> [my_bool]

(* Exemples de tests :
Pour un arbre : 
liste_feuille (cons_arbre (table 25899 16));;
Pour un sous-arbre :
let Noeud(_, sag, _) = cons_arbre (table 25899 16) in liste_feuille sag
*)
