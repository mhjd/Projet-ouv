
reset 
set terminal png
set output "Images/taille.png"
set xlabel "Taille (log du nombre de bits)"
set ylabel "Taille après compression (nombre de noeud)"
plot "compression_taille.txt" using 1:2 with lines title "compression par liste", "compression_taille.txt" using 1:3 with lines title "compression par arbre"

reset 
set terminal png
set output "Images/temps.png"
set xlabel "Taille (log du nombre de bits)"
set ylabel "Temps (en secondes)"
plot "compression_temps.txt" using 1:2 with lines title "compression par liste", "compression_temps.txt" using 1:3 with lines title "compression par arbre"

reset 
set terminal png
set output "Images/distribution.png"
set xrange [0:1023]
set yrange [0:0.05]
set ylabel "probabilite"
set xlabel "taille de l'arbre compresse'"
plot "distribution_probabilite.txt" title "" with impulses