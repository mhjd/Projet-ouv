open Compression
open Arbre_decision
open Grands_entiers


let format_print_graphe f g = 
  match !g with 
  | Feuille (booleen) -> 
    if booleen then Printf.fprintf f "\n%d [shape = box, label = True,color =green];" (Obj.magic g)
    else Printf.fprintf f "\n%d [shape = box, label = False,color =red];" (Obj.magic g) 

  | Noeud(profondeur, fg, fd) -> Printf.fprintf f "\n%d [label = %d];" (Obj.magic g) profondeur ;
    Printf.fprintf f "\n%d -> %d [style=dotted];" (Obj.magic g) (Obj.magic fg);
    Printf.fprintf f "\n%d -> %d;" (Obj.magic g) (Obj.magic fd);
  ;;




let dot (nom : string) (g : arbre_decision ref) : unit =
  let f = open_out nom in (*Ouverture du fichier où on met le graphe*)

  (* *)
    (*Fonction locale permettant de mettre tous les noeuds de l'arbre au bon format en faisant un parcourt du graphe*)
    let rec print_graphe (graphe : arbre_decision ref) (bordure : arbre_decision ref list) (vus : arbre_decision ref list): unit = 
    if (not (List.memq graphe vus)) then format_print_graphe f graphe ;  (*Si on n'a pas déjà imprimé ce noeud, on le fait*)
     match (!graphe, bordure) with 
      | (Feuille(booleen),[])-> ()    (*On a fini la bordure et on est sur un noeud sans fils*)
      | (Feuille(_),h::tl) -> (print_graphe h tl (graphe::vus))   (*Noeud sans fils mais toujours des noeuds dans la bordure*)
      | (Noeud(_,fg,fd),_) -> (print_graphe fg (fd::bordure) (graphe::vus)) (*Noeud avec fils*)
            
    in 
      Printf.fprintf f "digraph g {\n";   (*Préambule*)
      print_graphe g [] [] ; 
      Printf.fprintf f "}\n";
  close_out f;
;;

let (g,_) = (compressionParListe (cons_arbre (table 25899 16))   []) in dot "essai.dot" g ;;

let g = cons_arbre (table 25899 16) in dot "arbre_non_comp.dot" (ref g);;