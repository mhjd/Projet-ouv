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

let test_compression algo =
  let my_tree = cons_arbre (table (genAlea(63)) 16)
  in let my_time = time_of (algo my_tree)
  in my_time
 
;;

Printf.printf "Temps d'exécution de la compression par arbre : %f \n" (test_compression compressionParArbre);;
Printf.printf "Temps d'exécution de la compression par liste : %f" (test_compression compressionParListe);;

