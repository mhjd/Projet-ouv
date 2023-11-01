#!/bin/bash

    if (make dot > trace_compilation.txt) then 

    echo "La compilation a reussi ! On peut lancer l'exécution"

    ./dot.exe


    dot -Tpng arbre_comp_l.dot -o Images/arbre_compresse_par_liste.png
    dot -Tpng arbre_comp_t.dot -o Images/arbre_compresse_par_arbre.png
    dot -Tpng arbre_non_comp.dot -o Images/arbre_non_compresse.png
    make clean 
    else 
        echo "Attention ! Vous n'avez peut-être pas la bonne version de OCaml, il faut utiliser la version 4.14."
    fi