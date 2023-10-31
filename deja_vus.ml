open Grands_entiers
open Arbre_decision

(*Ce fichier contient le code permettant d'utiliser des modules dans notre algorithme de compression :
   * SetDejaVu est un type de module définis par la signature décrite ci-dessous
   * SetList est une implémentation de ce module où l'ensemble est représenté par une liste
   * SetTree est une implémentation de ce module où l'ensemble est représenté par un arbre
   
   Ces modules permettent de factoriser le code des parties 3 et 4*)


module type SetDejaVu = sig
  type ens
  val empty : ens
  val insert : (entier_precis * arbre_decision ) -> ens -> ens
  val mem : entier_precis -> ens-> (arbre_decision option)
  end;;


(*Question 10*)
(*Module encapsulant la structure de données demandée (définie par le type t)*)
module SetList : SetDejaVu = struct
  type elemListeDejaVus = entier_precis * arbre_decision  
  type ens = elemListeDejaVus list
  let empty = []
  let rec insert (couple : entier_precis * arbre_decision) (l: ens) = couple::l
  let rec mem (grd_entier : entier_precis) (l: ens) : (arbre_decision option) = 
    match l with
  | (tete_grand_entier, pointeur_node)::queue -> if tete_grand_entier = grd_entier then Some(pointeur_node) else (mem grd_entier queue)
  | [] -> None
end;;

(*Question 15*)
(*Module encapsulant la structure de données demandée (définie par le type t)*)
module SetTree : SetDejaVu = struct
  type ens =
    | Node of (arbre_decision option) * ens * ens
    | Leaf
  let empty = Leaf

  
  let rec insert (couple : entier_precis * arbre_decision) (ensemble : ens) : ens =
    let (e_p, pointeur) = couple in  
    let rec aux (liste_booleens : bool list ) (arbre: ens) = 
      match arbre with
        | Node(ma_val, sag, sad) -> (
            match liste_booleens with
            | true::tl -> Node(ma_val, sag, (aux tl sad ))
            | false::tl  -> Node(ma_val, (aux tl sag), sad ) 
            | [] -> Node(Some(pointeur), sag, sad)
          )
        | Leaf -> (
            match liste_booleens with
            | true::tl -> Node(None, Leaf, (aux tl Leaf) )
            | false::tl -> Node(None, (aux tl Leaf ), Leaf)
            | [] -> Node(Some(pointeur), Leaf, Leaf)
          )
    in (aux (decomposition e_p) ensemble)

  let mem (grd_entier : entier_precis) (ensemble : ens) : (arbre_decision option)= 
    let rec aux (liste_booleens : bool list) (arbre : ens) : arbre_decision option = 
      match arbre with
      | Node(pointeur, sag, sad) -> (
          match liste_booleens with
          | false::tl -> aux tl sag
          | true::tl  -> aux tl sad
          | [] -> pointeur
        )
      | Leaf -> None
    in (aux (decomposition grd_entier) ensemble)
  end;;
