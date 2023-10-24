open Arbre_decision
open Grands_entiers


(* L'exo parle d'une liste d'entier précis * pointeur vers node, mais je pense que c'est juste par analogie avec le C
Genre on peut le faire en OCaml sans pointeur 
https://ocaml.org/docs/pointers
EDIT : en faite je pense que faut vraiment mettre un pointeur, sinon ça fait des copies et on se retrouve avec le même graphe/arbre
*)
type elemListeDejaVus = ElemListeDejaVus of entier_precis * arbre_decision ;;
type listeDejaVus = ListeDejaVus of elemListeDejaVus list ;;


(* est_contenu renvoie Some(pointeur) dans le cas où le sous arbre est déjà existant dans la liste déjà vu, et renvoie None dans le cas où c'est pas le cas*)
let rec est_contenu grand_entier ma_liste_deja_vu  = 
  match ma_liste_deja_vu with
  | (tete_grand_entier, pointeur_node)::queue -> if tete_grand_entier = grand_entier then Some(pointeur_node) else est_contenu grand_entier queue
  | [] -> None
;;


let rec compressionParListe graphe ma_liste = 
  begin 
    match graphe with
    | Noeud(profond, sag, sad) -> 
        let nouveau_sag, nouvelle_liste1 = compressionParListe sag ma_liste in
        let nouveau_sad, nouvelle_liste2 = compressionParListe sad nouvelle_liste1 in 
        let grand_entier = (composition64 (liste_feuille graphe)) in 
        begin 
          match est_contenu grand_entier nouvelle_liste2 with
          | Some(pointeur) -> (pointeur, nouvelle_liste2)
          | None -> (Noeud(profond, nouveau_sag, nouveau_sad), (grand_entier, graphe)::nouvelle_liste2)
        end    
    | Feuille(booleen) ->  graphe, ma_liste 
  end 
(*
Test :
compressionParListe (cons_arbre (table 25899 16))   []
*)

