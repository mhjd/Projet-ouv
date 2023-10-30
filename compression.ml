open Grands_entiers
open Arbre_decision
open Deja_vus


(* L'exo parle d'une liste d'entier précis * pointeur vers node, mais je pense que c'est juste par analogie avec le C
Genre on peut le faire en OCaml sans pointeur 
https://ocaml.org/docs/pointers
EDIT : en faite je pense que faut vraiment mettre un pointeur, sinon ça fait des copies et on se retrouve avec le même graphe/arbre
*)

module AlgoCompression (E : SetDejaVu) =
  struct

  (*Renvoie si la deuxième moitié de la liste de booléens est fausse*)
  let deuxieme_moitie_false (liste : bool list) : bool = 
    let moitie = (List.length liste )/2 in     (*On n'utilisera que des listes dont la taille est une puissance de 2, la div est tjrs entière*)
    let rec aux l i = 
      match l with 
      | [] -> true
      | h::tl -> if i < moitie then (aux tl (i+1))    (*On ne regarde que la seconde moitié de la liste !*)
          else if h then false                     (*Si h est vrai à ce stade, la fonction renvoie faux directement*)
          else (aux tl (i+1))
    in (aux liste 0)


  (*Fonction implémentant l'algorithme*)
  let compression (graphe : arbre_decision) : arbre_decision = 
    let rec aux (graphe : arbre_decision) (deja_vus : E.t) : (arbre_decision * E.t) = 
      (*valeurs utiles peu importe la forme de l'arbre*)
      let feuilles = liste_feuille graphe in 
      match graphe with
      | Noeud(profond, sag, sad) -> 
            (*Parcourt en profondeur de l'arbre avec mise à jour de la liste des éléments vus*)
          let nouveau_sag, n_deja_vus1 = aux sag deja_vus in
          let nouveau_sad, n_deja_vus2 = aux sad n_deja_vus1 in 
            (*Début de la compression*)
            if (deuxieme_moitie_false feuilles) then (nouveau_sag, n_deja_vus2)
            else 
            (let grand_entier = (composition64 feuilles) in 
              match (E.mem grand_entier n_deja_vus2) with     (*Consultation de la liste de noeuds visités*)
              | Some(pointeur) -> (*Règle de compression M *)   (pointeur, n_deja_vus2)                             (*On remplace pour le père ce noeud par l'autre qui lui correspond*)
              | None -> (*Pas de compression à ce niveau*) 
                let nouveau_noeud =   (Noeud (profond,nouveau_sag,nouveau_sad))  (*On fait un nouveau noeud*)
                  in 
                  ( nouveau_noeud , (E.insert (grand_entier, nouveau_noeud) n_deja_vus2))      (*On retourne le couple graphe / liste dont la tête est un couple grand entier/noeud associé*)
            )
      | Feuille(booleen) ->  (*On évite la duplication des feuilles également*)
        (let grand_entier = (composition64 feuilles) in 

          match (E.mem grand_entier deja_vus) with     (*Consultation de la liste de noeuds visités*)
          | Some(pointeur) -> (*Règle de compression M *) 
            (pointeur, deja_vus)                             (*On remplace pour le père ce noeud par l'autre qui lui correspond*)
          | None -> (*Pas de compression à ce niveau*)
            ( graphe , (E.insert (grand_entier, graphe) deja_vus))      (*On retourne le couple graphe / liste dont la tête est un couple grand entier/noeud associé*)
            
        )
    in let (g,_) = (aux graphe E.empty) in g

  end ;;






