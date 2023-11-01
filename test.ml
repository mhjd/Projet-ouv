open Compression
open Arbre_decision
open Grands_entiers
open Deja_vus

let time_of f =
  let debut = Sys.time() in
  let _ = f in
  let fin = Sys.time() in
  (*étrange, faut mettre -. pour faire une substraction entre float*)
  fin -. debut
;;
(* j'ai mis un float en sortie, car ça permet de l'utiliser dans test_compression
 j'aurais pu laisser en int, et passer la fonction + ou +. en argument mais galère pour rien*)
let rec size_of (t:arbre_decision):float=
  match t with
  | Noeud (_, sag, sad) -> 1.0 +. size_of sag +. size_of sad
  | Feuille(_) -> 0.0
;;
    
(* À l'air de fonctionné :  *)
(* Printf.printf "taille d'un arbre 16 : %f \n " (size_of (cons_arbre(table (genAlea(16)) 16))) *)



let test_compression algo_de_test algo_compression  n max_bits =
  let data_list = ref [] in
  let bits = ref 2 in
  while !bits < max_bits do
    let total = ref 0.0 in
    for nb_test = 0 to n do
       let my_tree = cons_arbre (table (genAlea(!bits)) !bits)
       in let my_data = algo_de_test (algo_compression my_tree)
       in total := !total +. my_data
    done;
    data_list := (!total)::!data_list;
    bits := (!bits)*2
  done;
    List.rev !data_list
;;

let rec test_compression_temps = test_compression time_of ;;
let rec test_compression_taille = test_compression size_of;;


let rec gen_liste (n:int):int list =
  let rec aux i n_plus_un =
  if i < n_plus_un then
    i::(aux (i + 1) n_plus_un)
  else
    []
  in aux 1 (n+1)

let file = open_out "compression_temps.txt";;

let data2 = (test_compression_temps compressionParArbre 200 500);;
let data1 = (test_compression_temps compressionParListe 200 500);;
let taille = gen_liste (List.length data1);;


let rec write_data_to_file data1 data2 abs=
  match (data1, data2, abs) with
  | ([], [], []) -> ()
  | (x1::rest1, x2::rest2, abs1::rest3) ->
    Printf.fprintf file "%d %f %f\n" abs1 x1 x2 ;
    write_data_to_file rest1 rest2 rest3
  | _ -> failwith "Les listes n'ont pas la même longueur"
in
write_data_to_file data1 data2 taille;

close_out file;;
(*
  set title "Comparaison du temps d'exécution"
  set xlabel "Taille (log du nombre de bits)"
  set ylabel "Temps (en secondes)"
  plot "compression_temps.txt" using 1:2 with lines title "compression par liste", "compression_temps.txt" using 1:3 with lines title "compression par arbre"
 *)
let file = open_out "compression_taille.txt";;

let data2 = (test_compression_taille compressionParArbre 200 500);;
let data1 = (test_compression_taille compressionParListe 200 500);;
let taille = gen_liste (List.length data1);;


let rec write_data_to_file data1 data2 abs=
  match (data1, data2, abs) with
  | ([], [], []) -> ()
  | (x1::rest1, x2::rest2, abs1::rest3) ->
    Printf.fprintf file "%d %f %f\n" abs1 x1 x2 ;
    write_data_to_file rest1 rest2 rest3
  | _ -> failwith "Les listes n'ont pas la même longueur"
in
write_data_to_file data1 data2 taille;

close_out file;;

(*
  set title "Comparaison de la taille après compression"
  set xlabel "Taille (log du nombre de bits)"
  set ylabel "Taille après compression (nombre de noeud)"
  plot "compression_taille.txt" using 1:2 with lines title "compression par liste", "compression_taille.txt" using 1:3 with lines title "compression par arbre"
*)



