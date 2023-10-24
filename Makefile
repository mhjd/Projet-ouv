

all : grands_entiers.cmo arbre_decision.cmo compression.cmo tests.cmo tests


tests :  grands_entiers.cmo arbre_decision.cmo compression.cmo tests.cmo
	ocamlc grands_entiers.cmo  arbre_decision.cmo  compression.cmo tests.cmo -o tests.exe

proposition.cmo : 
	ocamlc -c proposition.ml

grands_entiers.cmo : 
	ocamlc -c grands_entiers.ml

arbre_decision.cmo : 
	ocamlc -c arbre_decision.ml

compression.cmo : 
	ocamlc -c compression.ml

tests.cmo : 
	ocamlc -c tests.ml


clean : 
	rm -f *.cmo *.cmi *.exe