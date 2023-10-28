

(* Renvoie le nouvel arbre compressé et 
  la liste des couples (bigint, bdd) des noeuds visités. *)
(* bdd -> listDejaVus -> (bdd, listDejaVus) *)
let rec compressionParListeAux current_node ldv =
  let (new_node, ldv) =
    match current_node with
    | Leaf(e) -> (current_node, ldv)
    | Node(g, e, d) -> 
        let (g1, ldv) = 
          match !g with
          | Leaf(_) -> (g, ldv)
          | Node(_, _, _) -> compressionParListeAux (!g) ldv
        in
        let (d1, ldv) = 
          match !d with
          | Leaf(_) -> (d, ldv)
          | Node(_, _, _) -> compressionParListeAux (!d) ldv
        in
        let (g1_treated, ldv) = treatNodeCompression (!g1) ldv in
        let (d1_treated, ldv) = treatNodeCompression (!d1) ldv in
        (* Printf.printf "fils égaux == ? %b\n" (g1_treated == d1_treated); *)
        (* Printf.printf "ref fils égales = ? %b\n" (ref g1_treated = ref d1_treated); *)
        let new_node = Node(g1_treated, e, d1_treated) in
        (new_node, ldv)
  in
  (ref new_node, ldv)
;;

(* Si la val du sous-arbre est dans ldv, 
  renvoie une ref vers sa seconde composante, et ldv inchangée.
  Sinon renvoie une ref vers le noeud, et ldv à laquelle (val, ref noeud) a été ajouté. *)
  let treatNodeCompression current_node ldv =
    let n = composition (liste_feuilles current_node) in
    let seconde_comp = (get_second_componant n (=) ldv) in
    match seconde_comp with
    | None -> 
        (* Printf.printf "Nouveau bigint \n" ;  *)
        let pointeur = ref current_node in
        (pointeur, (n, pointeur)::ldv)
    | Some pointeur -> 
        (* Printf.printf "Old bigint \n" ;  *)
        (pointeur, ldv)
  ;;
