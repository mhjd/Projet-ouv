open Grands_entiers
open Arbre_decision
open Deja_vus


(*Question 16 (adapter l'algorithme élémentaire se fait grâce au foncteur)*)
(*Module gérant la compression, paramétré par le type de structure utilisé pour représenter l'ensemble des
   noeuds déjà vus.*)
module AlgoCompression (E : SetDejaVu) =
  struct

  (*Fonction auxiliaire, renvoie si la deuxième moitié de la liste de booléens est fausse*)
  let deuxieme_moitie_false (liste : bool list) : bool = 
    let moitie = (List.length liste )/2 in     (*On n'utilisera que des listes dont la taille est une puissance de 2, la div est tjrs entière*)
    let rec aux l i = 
      match l with 
      | [] -> true
      | h::tl -> if i < moitie then (aux tl (i+1))    (*On ne regarde que la seconde moitié de la liste !*)
          else if h then false                     (*Si h est vrai à ce stade, la fonction renvoie faux directement*)
          else (aux tl (i+1))
    in (aux liste 0)


  (*Questions 11 et 17*)
  (*Fonction implémentant l'algorithme décrit dans l'énoncé*)
  let compression (arbre : arbre_decision) : arbre_decision = 
    let rec aux (arbre : arbre_decision) (deja_vus : E.ens) : (arbre_decision * E.ens) = 
      (*valeurs utiles peu importe la forme de l'arbre*)
      let feuilles = liste_feuille arbre in 
      match arbre with
      | Noeud(profond, sag, sad) -> 
            (*Parcourt en profondeur de l'arbre avec mise à jour de l'ensemble des éléments vus*)
          let nouveau_sag, n_deja_vus1 = aux sag deja_vus in
          let nouveau_sad, n_deja_vus2 = aux sad n_deja_vus1 in 

            (*Début de la compression*)
            (*Règle Z*)
            if (deuxieme_moitie_false feuilles) then (nouveau_sag, n_deja_vus2)
            else 
            (let grand_entier = (composition64 feuilles) in 
              match (E.mem grand_entier n_deja_vus2) with     (*Consultation de l'ensemble des noeuds visités*)
              | Some(pointeur) -> (*Règle de compression M *)   (pointeur, n_deja_vus2)                             (*On remplace pour le père ce noeud par l'autre qui lui correspond*)
              | None -> (*Pas de compression à ce niveau*) 
                let nouveau_noeud =   (Noeud (profond,nouveau_sag,nouveau_sad))  (*On fait un nouveau noeud*)
                  in 
                  ( nouveau_noeud , (E.insert (grand_entier, nouveau_noeud) n_deja_vus2))      (*On retourne le couple arbre /  ensemble où on a inséré le couple *)
            )

      | Feuille(booleen) ->  (*On évite la duplication des feuilles également*)
        (let grand_entier = (composition64 feuilles) in 

          match (E.mem grand_entier deja_vus) with     (*Consultation de l'ensemble des noeuds visités*)
          | Some(pointeur) -> (*Règle de compression M *) 
            (pointeur, deja_vus)                             (*On remplace pour le père ce noeud par l'autre qui lui correspond*)
          | None -> (*Pas de compression à ce niveau*)
            ( arbre , (E.insert (grand_entier, arbre) deja_vus))      (*On retourne le couple arbre / ensemble où on a inséré le couple*)
            
        )
    in let (g,_) = (aux arbre E.empty) in g

  end ;;

  (*Ces modules sont les applications du foncteur AlgoCompression à SetList et SetTree, et permettent chacun d'appeler
   l'algorithme défini par le foncteur avec leur structure*)
  (*Question 11*)
  module FL = AlgoCompression (SetList);;     
  let compressionParListe = FL.compression;;

  (*Question 17*)
  module FT = AlgoCompression (SetTree);;
  let compressionParArbre = FT.compression;;







