open Grands_entiers
open Arbre_decision


module type SetDejaVu = sig
  type t
  val empty : t
  val insert : (entier_precis * arbre_decision ) -> t -> t
  val mem : entier_precis -> t -> (arbre_decision ) option
  end;;


module SetList : SetDejaVu = struct
  type elemListeDejaVus = entier_precis * arbre_decision  ;;

  type t = elemListeDejaVus list
  let empty = []
  let rec insert couple l = couple::l
  let rec mem grd_entier l = 
    match l with
  | (tete_grand_entier, pointeur_node)::queue -> if tete_grand_entier = grd_entier then Some(pointeur_node) else (mem grd_entier queue)
  | [] -> None
end;;
