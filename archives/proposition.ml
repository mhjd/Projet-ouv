open Grands_entiers

(*Code à améliorer, restent des choses qui ne marchent pas, mais en bon chemin (voué à remplacer compression.ml et arbre_decision.ml)*)


(*Question 7*)
type profondeur = int ;;

type arbre_decision =
  | Noeud of profondeur * arbre_decision ref * arbre_decision ref
  | Feuille of bool ;;
(* Exemple de test :
   Noeud(1, Noeud(1, Feuille(true), Feuille(false)), Feuille(true));;*)

(*Question 8*)
(*Fonction générant un arbre de décision associé à la table de vérité t
La fonction head permet d'éviter de devoir préciser une profondeur, devant toujours être égale à 1 lors de l'appel
Cela serait pertinent de créer un type table_de_verite, au lieu de directement utiliser "bool list", qui est pas très propre car on a d'autre liste de bool
*)


let new_pointer x = ref x ;;
let deref_pointer x = !x ;;
(*
compressionParListe( new_pointer (cons_arbre (table 25899 16))) [];;
*)
let cons_arbre (t : bool list) : arbre_decision =
  let rec cons_arbre_head (t : bool list) dpth =
    match t with 
    | [] -> failwith "Cas impossible normalement"
    | h1::h2::[] -> Noeud(dpth, (new_pointer (Feuille(h1))), (new_pointer (Feuille(h2))))
    | l -> let nb_feuilles = (List.length t) / 2 in
        let sous_arbre_droit = cons_arbre_head (completion l nb_feuilles) (dpth+1) in
        let sous_arbre_gauche = cons_arbre_head (suite_liste l nb_feuilles) (dpth+1) in
        Noeud(dpth, new_pointer sous_arbre_droit , new_pointer sous_arbre_gauche)
  in cons_arbre_head t 1
(* Exemple de test, repris de l'énoncé :
cons_arbre (table 25899 16) ;;
*) 

(*Question 9*)
(*Renvoie les feuilles d'un arbre ou un sous-arbre*)
let rec liste_feuille arbre =
  match arbre with
  | Noeud(dpth, sous_arbre_gauche, sous_arbre_droit) -> (liste_feuille (deref_pointer sous_arbre_gauche)) @ (liste_feuille (deref_pointer sous_arbre_droit))
  | Feuille(my_bool) -> [my_bool]

(* Exemples de tests :
Pour un arbre : 
liste_feuille (cons_arbre (table 25899 16));;
Pour un sous-arbre :
let Noeud(_, sag, _) = cons_arbre (table 25899 16) in liste_feuille sag
*)


(* L'exo parle d'une liste d'entier précis * pointeur vers node, mais je pense que c'est juste par analogie avec le C
Genre on peut le faire en OCaml sans pointeur 
https://ocaml.org/docs/pointers
EDIT : en faite je pense que faut vraiment mettre un pointeur, sinon ça fait des copies et on se retrouve avec le même graphe/arbre
*)



type arbre_decision_pointer = Null | Pointer of arbre_decision ref ;;
type elemListeDejaVus = ElemListeDejaVus of entier_precis * arbre_decision ref ;;
type listeDejaVus = ListeDejaVus of elemListeDejaVus list ;;


(* est_contenu renvoie Some(pointeur) dans le cas où le sous arbre est déjà existant dans la liste déjà vu, et renvoie None dans le cas où c'est pas le cas*)
let rec est_contenu grand_entier ma_liste_deja_vu  = 
  match ma_liste_deja_vu with
  | (tete_grand_entier, pointeur_node)::queue -> if tete_grand_entier = grand_entier then Some(pointeur_node) else est_contenu grand_entier queue
  | [] -> None
;;


let deuxieme_moitie_false (liste : bool list) :bool = 
  let moitie = (List.length liste )/2 in     (*On n'utilisera que des listes dont la taille est une puissance de 2, la div est tjrs entière*)
    let rec aux l i = 
        match l with 
        | [] -> true
        | h::tl -> if i < moitie then (aux tl (i+1))    (*On ne regarde que la seconde moitié de la liste !*)
        else if h then false                     (*Si h est vrai à ce stade, la fonction renvoie faux directement*)
        else (aux tl (i+1))
      in (aux liste 0);;





(*Version de Habib*)
let rec compressionParListe graphe ma_liste = 
  begin 
    match deref_pointer graphe with
    | Noeud(profond, sag, sad) -> 
        let nouveau_sag, nouvelle_liste1 = compressionParListe sag ma_liste in
        let nouveau_sad, nouvelle_liste2 = compressionParListe sad nouvelle_liste1 in 
        let grand_entier = (composition64 (liste_feuille (deref_pointer graphe))) in 

        if (deuxieme_moitie_false (liste_feuille (deref_pointer graphe))) then 
          ( nouveau_sag, nouvelle_liste2)
      else 
        begin 
          match est_contenu grand_entier nouvelle_liste2 with
          | Some(pointeur) -> (pointeur, nouvelle_liste2)
          | None -> (new_pointer (Noeud(profond, nouveau_sag, nouveau_sad)),  (grand_entier, graphe)::nouvelle_liste2)
        end    
    | Feuille(booleen) ->  (graphe, ma_liste )
  end 
(*
Test :
compressionParListe (cons_arbre (table 25899 16))   []
*)