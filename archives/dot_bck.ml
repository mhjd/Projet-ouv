open Compression
open Grands_entiers

exception Fin_de_visite_de_branche of (arbre_decision ref) list ;;


let format_print_graphe f g = 
  match !g with 
  | Feuille (booleen) -> 
    if booleen then Printf.fprintf f "\n%d [shape = box, label = True,color =green];" (Obj.magic g)
    else Printf.fprintf f "\n%d [shape = box, label = False,color =red];" (Obj.magic g) 

  | Noeud(profondeur, fg, fd) -> Printf.fprintf f "\n%d [label = %d];" (Obj.magic g) profondeur ;
    Printf.fprintf f "\n%d -> %d [style=dotted];" (Obj.magic g) (Obj.magic fg);
    Printf.fprintf f "\n%d -> %d;" (Obj.magic g) (Obj.magic fd);
  ;;

  

let dot_bck (nom : string) (g : arbre_decision ref) : unit =
  let f = open_out nom in (*Ouverture du fichier où on met le graphe*)
    (*Fonction locale permettant de mettre tous les noeuds de l'arbre au bon format*)
    let rec print_graphe (graphe : arbre_decision ref) (noeuds_visites : (arbre_decision ref) list) : unit = 
      try (

        if (not (List.mem graphe noeuds_visites) ) then (
          format_print_graphe f graphe ;
          (match !graphe with 
          | Feuille (_) -> raise (Fin_de_visite_de_branche (graphe::noeuds_visites));
          | Noeud(_, fg, fd) -> 
          try (print_graphe fg (graphe::noeuds_visites); raise (Fin_de_visite_de_branche (graphe::noeuds_visites))) with
            Fin_de_visite_de_branche nouvelle_liste -> 
              (try (print_graphe fd nouvelle_liste) with 
              Fin_de_visite_de_branche nouvelle_liste2 -> ())
          )
        )
        else 
          raise (Fin_de_visite_de_branche noeuds_visites)
      )
      with Fin_de_visite_de_branche l -> ()
    in 
      Printf.fprintf f "digraph g {\n";   (*Préambule*)
      print_graphe g []; 
      Printf.fprintf f "}\n";
  close_out f;
  ;;  

