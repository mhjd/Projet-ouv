open Grands_entiers
open Arbre_decision


module type SetDejaVu = sig
  type t
  val empty : t
  val insert : (entier_precis * arbre_decision ) -> t -> t
  val mem : entier_precis -> t -> (arbre_decision option)
  end;;


module SetList : SetDejaVu = struct
  type elemListeDejaVus = entier_precis * arbre_decision  

  type t = elemListeDejaVus list
  let empty = []
  let rec insert couple l = couple::l
  let rec mem (grd_entier : entier_precis) (l: t) : (arbre_decision option) = 
    match l with
  | (tete_grand_entier, pointeur_node)::queue -> if tete_grand_entier = grd_entier then Some(pointeur_node) else (mem grd_entier queue)
  | [] -> None
end;;


module SetTree : SetDejaVu = struct
  type t =
    | Node of (arbre_decision option) * t * t
    | Leaf
  let empty = Leaf
  let rec insert (couple : entier_precis * arbre_decision) (ensemble : t) : t =
    let (e_p, pointeur) = couple in  
    let (e_p_booleen) = decomposition e_p in

    let rec aux (liste_booleens : bool list ) (arbre: t) = 
      match arbre with
        | Node(ma_val, sag, sad) -> (
            match liste_booleens with
            | true::t -> Node(ma_val, sag, (aux t sad ))
            | false::t  -> Node(ma_val, (aux t sag), sad ) 
            | [] -> Node(Some(pointeur), sag, sad)
          )
        | Leaf -> (
            match liste_booleens with
            | true::t -> Node(None, Leaf, (aux t Leaf) )
            | false::t -> Node(None, (aux t Leaf ), Leaf)
            | [] -> Node(Some(pointeur), Leaf, Leaf)
          )
    in (aux e_p_booleen ensemble)

  let mem (grd_entier : entier_precis) (l : t) : (arbre_decision option)= 
    let grd_entier_booleen = decomposition grd_entier in 
    let rec aux (liste_booleens : bool list) (arbre : t) : arbre_decision option = 
      match arbre with
      | Node(pointeur, sag, sad) -> (
          match grd_entier_booleen with
          | false::t -> aux t sad 
          | true::t  -> aux t sag
          | [] -> pointeur
        )
      | Leaf -> None
    in (aux grd_entier_booleen l)
  end;;
