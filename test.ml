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
let test_compression algo n max_bits=

  let rec my_loop_bits n_remaining bits =
     let rec my_loop_time n_remaining bits acc = 
       let my_tree = cons_arbre (table (genAlea(bits)) bits)
       in let my_time = time_of (algo my_tree)
          in
          if n_remaining > 0 then
            my_loop_time bits (n_remaining-1) my_time+.acc
          else 0.0
     in (my_loop_time n max_bits 0.0) /. (float_of_int n)
  in my_loop_bits 10
;;

let fois_deux x =
  x*2
let test_compression algo n max_bits=
  let time_list = ref [] in
  let bits = ref 2 in 
  while !bits < max_bits do
    Printf.printf "bits : %d \n" !bits;
    let total = ref 0.0 in 
    for nb_test = 0 to n do

       (* Printf.printf "numéro de test : %d \n" nb_test; *)
       let my_tree = cons_arbre (table (genAlea(!bits)) !bits)
       in let my_time = time_of (algo my_tree)
       in total := !total +. my_time 
    done;
    (* Printf.printf "temps moyen est de %f pour %d bits \n " !total  !bits ;  *)
    time_list := (!total (* /. (float_of_int n) *))::!time_list;
    bits := fois_deux (!bits)
  done; 
    !time_list
;;

let file = open_out "compression.txt";;
let rec gen_liste n =
  if n > 0 then 
    n::(gen_liste (n - 1))
  else []

let data2 = (test_compression compressionParArbre 200 1200);;
let data1 = (test_compression compressionParListe 200 1200);;
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

(*plot "compression.txt" using 1:2 with lines title "compression par liste", "compression.txt" using 1:3 with lines title "compression par arbre"
*)


(* print_string "Compression par arbre : \n " ; *)
(* List.iter (Printf.printf "%f ,") (test_compression compressionParArbre 100 600); *)
(* print_string "Compression par liste : \n " ; *)
(* List.iter (Printf.printf "%f ,") (test_compression compressionParListe 100 600) *)

(* Printf.printf "Temps d'exécution de la compression par arbre : %f \n" (test_compression compressionParArbre 2 20);; *)
(* Printf.printf "Temps d'exécution de la compression par liste : %f" (test_compression compressionParListe 2 20);; *)

