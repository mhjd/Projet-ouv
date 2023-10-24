open Int64;;


(* Question 1 *)
(*Définition du type entiers précis*)
type entier_precis = int64 list;;

(*Primitive permettant d'ajouter un élément à la fin d'une liste*)
let ajout_fin (l:'a list) (x:'a) : 'a list= 
  let rec aux (l : 'a list ) (acc : 'a list) = 
    match l with 
    | [] -> acc
    | hd :: tl -> (aux tl (hd::acc))
  in aux (List.rev l) [x];;

(*Primitive permettant de récupérer la valeur en tête d'une liste*)
let recup_tete (l : 'a list) : 'a = 
  match l with
  | [] -> failwith "Liste vide !"
  | hd::tl -> hd;;

(*Primitive permettant de récupérer la liste l sans sa tête*)
let list_decapit l = 
  match l with
  | [] -> []
  | hd::tl -> tl;;


(* Question 2 *)
(*Fonction auxiliaire permettant de changer un entier 64 bits en booléen (s'il est sur 1 bit)*)
let to_bool (x:int64):bool = match x with 0L -> false | 1L -> true | _-> failwith "Cas théoriquement impossible"

(*Fonction auxiliaire permettant de décomposer un entier 64 bits en liste de booléens*)
let rec decompose_entier64 (hd : int64) : bool list = 
  match hd with 
  | 0L -> [false]
  | 1L -> [true]
  | _ -> let bit = unsigned_rem hd 2L and suite = unsigned_div hd 2L in (to_bool bit)::(decompose_entier64 suite);;

(*Fonction permettant de décomposer un entier précis (ie liste d'entiers 64bits) en liste de booléens*)
let rec decomposition (x : entier_precis) : bool list =
  match x with 
  | [] -> []
  | hd::tl -> (decompose_entier64 hd)@(decomposition tl);;


(* Question 3 *)
(*Fonction auxiliaire permettant de tronquer une liste l pour qu'elle fasse au maximum n éléments*)
let rec tronquer (l:'a list) (n : int) (acc : 'a list) : 'a list = 
  if n = 0 then List.rev acc else 
    (match l with 
    |[]-> List.rev acc
    |(hd::tl) -> tronquer tl (n-1) (hd::acc) ) ;;

(*Fonction auxiliaire permettant d'ajouter n elem à la fin de la liste l*)
let rec ajouter_n_droite (l : 'a list) (n : int) (elem: 'a) (acc :'a list): 'a list = 
  if n = 0 then l@acc else (ajouter_n_droite l (n-1) elem (elem::acc));;

(*Fonction renvoyant soit la list tronquée ne contenant que les n premiers éléments, soit la liste complétée à droite
   de taille n, par des valeurs false*)
let completion (l:bool list) (n : int) : bool list = 
  if (List.length l > n ) then tronquer l n []
  else ajouter_n_droite l (n - List.length l) false [];;


(*Question 4*)

(*Fonction auxiliaire renvoyant la liste privée de ses n premiers éléments*)
let rec suite_liste (l : 'a list) (n : int ) : 'a list = 
  match l with 
  | [] -> []
  | hd :: tl -> if n <= 1 then tl else suite_liste tl (n-1);;

(*Fonction auxiliaire recomponsant un entier 64 bits à partir de sa bitmap (compris comme non signé)*)
let recompose_int64 (l : bool list): int64 = 
  let rec aux (l : bool list) (pow2cour : int64) (acc : int64) : int64 =
    match l with 
    | [] -> acc
    | hd::tl -> if hd then (aux tl (mul 2L pow2cour) ( add acc pow2cour )) else (aux tl (mul 2L pow2cour) acc)
  in (aux l 1L 0L);;

(*Fonction qui construit l'entier représenté en liste (en base 10) dont la liste de bits correspond à l'écriture binaire
   (sz sert à choisir la taille en nombre de bits des entiers de la liste )*)
let rec composition (sz : int) (l: bool list)  : entier_precis = 
  let courant = (recompose_int64 (completion l sz)) and suite = suite_liste l sz in
  match suite with 
  | [] -> [courant]
  | _-> (courant) :: (composition sz suite);;

let composition64 = composition 64;;


(*Question 5*)
(*Fonction qui décompose x en base 2 et complète la liste obtenue afin qu'elle soit de taille n (=> table de vérité)*)
let table x n = completion (decomposition [Int64.of_int x] ) n;;
  

(*Question 6*)
(*Fonction générant un grand entier aléatoire de n bits au maximum*)

(*Cette version pose problème car avec Random.int64 on est bornés par le max codable sur 64 bits en entier signés, 
  nous ne pouvons pas faire autrement avec Random*)
  let genAlea (n : int) : entier_precis = 
    let rec aux (n : int) (acc : entier_precis) = 
      if n = 0 then acc else (aux (n-1) ((Random.int64 Int64.max_int) ::acc))
    in let l = n/64 in let binf = n - l*64 in (aux l [(Random.int64 (Int64.of_int binf) )]);;
  
(* Cette version ne marche pas avant OCaml 4.14... On ne l'a pas encore dans les toplevel
let genAlea (n : int) : entier_precis = 
  let rec aux (n : int) (acc : entier_precis) = 
    if n = 0 then acc else (aux (n-1) (Random.bits64()::acc))
  in let l = n/64 in let binf = n - l*64 in (aux l (Random.int64 binf ));;
*)


(*Question 7*)
type profondeur = int ;;

type arbre_decision =
| Noeud of profondeur * arbre_decision * arbre_decision
| Feuille of bool ;;
(* Exemple de test :
   Noeud(1, Noeud(1, Feuille(true), Feuille(false)), Feuille(true));;*)

(*Question 8*)
(*Fonction générant un arbre de décision associé à la table de vérité t
La fonction head permet d'éviter de devoir préciser une profondeur, devant toujours être égale à 1 lors de l'appel
Cela serait pertinent de créer un type table_de_verite, au lieu de directement utiliser "bool list", qui est pas très propre car on a d'autre liste de bool
*)

let cons_arbre (t : bool list) : arbre_decision =
  let rec cons_arbre_head (t : bool list) dpth =
    match t with 
    | [] -> failwith "Cas impossible normalement"
    | h1::h2::[] -> Noeud(dpth, Feuille(h1), Feuille(h2))
    | l -> let nb_feuilles = (List.length t) / 2 in
        let sous_arbre_droit = cons_arbre_head (completion l nb_feuilles) (dpth+1) in
        let sous_arbre_gauche = cons_arbre_head (suite_liste l nb_feuilles) (dpth+1) in
        Noeud(dpth, sous_arbre_droit , sous_arbre_gauche)
  in cons_arbre_head t 1
(* Exemple de test, repris de l'énoncé :
cons_arbre (table 25899 16) ;;
*) 

(*Question 9*)
(*Renvoie les feuilles d'un arbre ou un sous-arbre*)
let rec liste_feuille arbre =
  match arbre with
  | Noeud(dpth, sous_arbre_gauche, sous_arbre_droit) -> (liste_feuille sous_arbre_gauche) @ (liste_feuille sous_arbre_droit)
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
*)
type elemListeDejaVus = ElemListeDejaVus of entier_precis * arbre_decision ;;
type listeDejaVus = ListeDejaVus of elemListeDejaVus list ;;
