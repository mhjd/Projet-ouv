open Compression
open Arbre_decision
open Grands_entiers
open Deja_vus



let time_of f =
  let debut = Sys.time() in
  let _ = f in
  let fin = Sys.time() in
  fin -. debut
;;

(*Note : on a fait une fonction qui calcule en flottant pour éviter les conversions par la suite...*)
let rec size_of (t:arbre_decision):float=
  match t with
  | Noeud (_, sag, sad) -> 1.0 +. size_of sag +. size_of sad
  | Feuille(_) -> 0.0
;;
    


(*Question 20*)
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




(*Question 21*)
(*Calcul de la distribution de la taille de la table*)
let rec sz (a : arbre_decision) : int = 
  match a with 
  | Feuille(_) -> 1
  | Noeud (_, fg, fd) -> 1 + (sz fg) + (sz fd);;
let distribution_probabilite (n : int) (occur : int): int Array.t = 
  let a = Array.make (2*n -1) 0 in 
  for i = 0 to occur-1 do 
    let sz = (sz (compressionParListe (cons_arbre (table (genAlea n) n))))-1
    in  a.(sz) <- a.(sz)+1
  done;
  a;;

Random.self_init;;

let print_distrib (n : int) (occur : int): unit= 
  let file = open_out "distribution_probabilite.txt" in
  let a = (distribution_probabilite n occur) in 
  for i = 0 to (Array.length a)-1 do 
    Printf.fprintf file "%d\t%f\n" (i+1) ((Int.to_float a.(i))/. (Int.to_float occur));
  done;
  close_out file;;

print_distrib 512 1000;;





