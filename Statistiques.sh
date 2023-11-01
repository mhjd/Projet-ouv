#!/bin/bash

    if (make test > trace_compilation.txt) then 

    echo "La compilation a reussi ! On peut lancer l'exécution"

    ./test.exe

    make clean 
    else 
        echo "Attention ! Vous n'avez peut-être pas la bonne version de OCaml, il faut utiliser la version 4.14."
    fi