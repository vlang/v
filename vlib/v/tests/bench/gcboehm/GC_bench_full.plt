#!/usr/bin/gnuplot -persist
set title "Boehm-GC: Optimized vs. non-Optimized (Full Mode)" 
set xlabel "Interval #" 
set ylabel "Pause Time [ms]" 
set terminal pdfcairo  transparent enhanced fontscale 0.5 size 5.00in, 3.00in 
set output "GC_bench_full.pdf"
plot "boehm_full.txt" title "full GC" w i, "boehm_full_opt.txt" title "full GC (opt)" w i
set output
#    EOF
