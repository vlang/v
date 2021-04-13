#!/usr/bin/gnuplot -persist
set title "Boehm-GC: Classic vs. Optimized Modes" 
set xlabel "Interval #" 
set ylabel "Pause Time [ms]" 
set terminal pdfcairo  transparent enhanced fontscale 0.5 size 5.00in, 3.00in 
set output "GC_bench.pdf"
plot "boehm_full.txt" title "full GC" w i lt 6, "boehm_incr_opt.txt" title "incr/generational GC (opt)" w i lt 2, "boehm_full_opt.txt" title "full GC (opt)" w i lt 7
set output
#    EOF
