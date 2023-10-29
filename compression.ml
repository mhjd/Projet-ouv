open Grands_entiers
open Arbre_decision
open Deja_vus


module type SetDejaVu = sig
  type t
  val empty : t
  val insert : (entier_precis * arbre_decision ) -> t -> t
  val mem : entier_precis -> t -> (arbre_decision ) option
  end




type elemListeDejaVus = entier_precis * arbre_decision  ;;
module SetList : SetDejaVu = struct
  type t = elemListeDejaVus list
  let empty = []
  let rec insert couple l = couple::l
  let rec mem grd_entier l = 
    match l with
  | (tete_grand_entier, pointeur_node)::queue -> if tete_grand_entier = grd_entier then Some(pointeur_node) else (mem grd_entier queue)
  | [] -> None
end





(* L'exo parle d'une liste d'entier précis * pointeur vers node, mais je pense que c'est juste par analogie avec le C
Genre on peut le faire en OCaml sans pointeur 
https://ocaml.org/docs/pointers
EDIT : en faite je pense que faut vraiment mettre un pointeur, sinon ça fait des copies et on se retrouve avec le même graphe/arbre
*)



(* est_contenu renvoie Some(pointeur) dans le cas où le sous arbre est déjà existant dans la liste déjà vu, et renvoie None dans le cas où c'est pas le cas*)


(*Renvoie si la deuxième moitié de la liste de booléens est fausse*)
let deuxieme_moitie_false (liste : bool list) : bool = 
  let moitie = (List.length liste )/2 in     (*On n'utilisera que des listes dont la taille est une puissance de 2, la div est tjrs entière*)
  let rec aux l i = 
    match l with 
    | [] -> true
    | h::tl -> if i < moitie then (aux tl (i+1))    (*On ne regarde que la seconde moitié de la liste !*)
        else if h then false                     (*Si h est vrai à ce stade, la fonction renvoie faux directement*)
        else (aux tl (i+1))
  in (aux liste 0);;




(*Fonction implémentant l'algorithme*)
let rec compressionParListe (graphe : arbre_decision) ma_liste  = 
  (*valeurs utiles peu importe la forme de l'arbre*)
  let feuilles = liste_feuille graphe in 
  let grand_entier = (composition64 feuilles) in 
  match graphe with
  | Noeud(profond, sag, sad) -> 
        (*Parcourt en profondeur de l'arbre avec mise à jour de la liste des éléments vus*)
      let nouveau_sag, nouvelle_liste1 = compressionParListe sag ma_liste in
      let nouveau_sad, nouvelle_liste2 = compressionParListe sad nouvelle_liste1 in 
        (*Début de la compression*)
        (*On avait implanté ici la règle Z, mais elle ne sert à rien.*)

      (match (SetList.mem grand_entier nouvelle_liste2) with     (*Consultation de la liste de noeuds visités*)
        | Some(pointeur) -> (*Règle de compression M *)   (pointeur, nouvelle_liste2)                             (*On remplace pour le père ce noeud par l'autre qui lui correspond*)
        | None -> (*Pas de compression à ce niveau*) 
          let nouveau_noeud =   (Noeud (profond,nouveau_sag,nouveau_sad))  (*On fait un nouveau noeud*)
            in 
            ( nouveau_noeud , (SetList.insert (grand_entier, nouveau_noeud) nouvelle_liste2))      (*On retourne le couple graphe / liste dont la tête est un couple grand entier/noeud associé*)
      )
  | Feuille(booleen) ->  (*On évite la duplication des feuilles*)
    (match (SetList.mem grand_entier ma_liste) with     (*Consultation de la liste de noeuds visités*)
      | Some(pointeur) -> (*Règle de compression M *) 
        (pointeur, ma_liste)                             (*On remplace pour le père ce noeud par l'autre qui lui correspond*)

      | None -> (*Pas de compression à ce niveau*) 
        
        ( graphe , (SetList.insert (grand_entier, graphe) ma_liste))      (*On retourne le couple graphe / liste dont la tête est un couple grand entier/noeud associé*)
         
    )
(*
Test :
compressionParListe (cons_arbre (table 25899 16))   []
*)






