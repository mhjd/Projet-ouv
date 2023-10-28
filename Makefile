

all : clean grands_entiers.cmo compression.cmo  dot.cmo  dot


dot :  grands_entiers.cmo  compression.cmo dot.cmo
	ocamlc grands_entiers.cmo   compression.cmo  dot.cmo -o dot.exe

grands_entiers.cmo : 
	ocamlc -c grands_entiers.ml

compression.cmo : 
	ocamlc -c compression.ml

dot.cmo : 
	ocamlc -c dot.ml


clean : 
	rm -f *.cmo *.cmi *.exe