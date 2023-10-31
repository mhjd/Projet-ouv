

all : clean grands_entiers.cmo arbre_decision.cmo deja_vus.cmo compression.cmo  dot.cmo  dot test


dot :  grands_entiers.cmo arbre_decision.cmo deja_vus.cmo compression.cmo dot.cmo
	ocamlc grands_entiers.cmo arbre_decision.cmo deja_vus.cmo compression.cmo  dot.cmo -o dot.exe

test : grands_entiers.cmo arbre_decision.cmo deja_vus.cmo compression.cmo test.cmo
	ocamlc grands_entiers.cmo arbre_decision.cmo deja_vus.cmo compression.cmo test.cmo -o test.exe


grands_entiers.cmo : 
	ocamlc -c grands_entiers.ml

arbre_decision.cmo :
	ocamlc -c arbre_decision.ml

deja_vus.cmo : 
	ocamlc -c deja_vus.ml

compression.cmo : 
	ocamlc -c compression.ml

dot.cmo : 
	ocamlc -c dot.ml

test.cmo :
	ocamlc -c test.ml

clean : 
	rm -f *.cmo *.cmi *.exe *.dot *.cmx *.o
