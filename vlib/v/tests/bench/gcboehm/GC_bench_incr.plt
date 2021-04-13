#!/usr/bin/gnuplot -persist
set title "Boehm-GC: Optimized vs. non-Optimized (Generational Mode)" 
set xlabel "Interval #" 
set ylabel "Pause Time [ms]" 
set terminal pdfcairo  transparent enhanced fontscale 0.5 size 5.00in, 3.00in 
set output "GC_bench_incr.pdf"
plot "boehm_incr.txt" title "non-optimized GC" w i, "boehm_incr_opt.txt" title "optimized GC" w i
set output
#    EOF
