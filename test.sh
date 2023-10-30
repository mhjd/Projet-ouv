#!/bin/bash

    if (make > trace_compilation.txt) then 

    echo "La compilation a reussi ! On peut lancer l'exécution"

    ./dot.exe


    dot -Tpng arbre_comp_l.dot -o Images/arbre_compresse.png
    dot -Tpng arbre_non_comp.dot -o Images/arbre_non_compresse.png
    else 
        echo "Attention ! Mauvaise version de OCaml ! Il faut la 4.14 !"
    fi