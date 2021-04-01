#!/usr/bin/gnuplot -persist
set title "Boehm-GC: Full vs. Incremental/Generational Mode" 
set xlabel "Interval #" 
set ylabel "Pause Time [ms]" 
set terminal pdfcairo  transparent enhanced fontscale 0.5 size 5.00in, 3.00in 
set output "GC_bench.pdf"
plot "boehm_full.txt" title "full GC" w i, "boehm_incr.txt" title "incr/generational GC" w i
set output
#    EOF
