open Compression
open Grands_entiers
open Dot_bck


let dot (nom : string) (g : arbre_decision ref) : unit =
  let f = open_out nom in (*Ouverture du fichier où on met le graphe*)


    (*Fonction locale permettant de mettre tous les noeuds de l'arbre au bon format*)
    let rec print_graphe (graphe : arbre_decision ref) : unit = 
        match !graphe with 
        | Feuille(booleen) -> (if booleen then Printf.fprintf f "\n%d [shape = box, label = True,color =green];" (Obj.magic graphe)
            else Printf.fprintf f "\n%d [shape = box, label = False,color =red];" (Obj.magic graphe)) ; 
        | Noeud(profondeur, fg, fd) -> Printf.fprintf f "\n%d [label = %d];" (Obj.magic graphe) profondeur ;
          Printf.fprintf f "\n%d -> %d [style=dotted];" (Obj.magic graphe) (Obj.magic fg);
          Printf.fprintf f "\n%d -> %d;" (Obj.magic graphe) (Obj.magic fd);
          
            print_graphe fg ;
            print_graphe fd;
    
    in 
      Printf.fprintf f "digraph g {\n";   (*Préambule*)
      print_graphe g; 
      Printf.fprintf f "}\n";
  close_out f;
;;

let (g,_) = (compressionParListe (cons_arbre (table 25899 16))   []) in dot_bck "essai_bck.dot" g ;;