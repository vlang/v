#!/usr/bin/gnuplot -persist
set title "Boehm-GC: Classic vs. Optimized Modes" 
set xlabel "Interval #" 
set ylabel "Pause Time [ms]" 
set terminal pdfcairo  transparent enhanced fontscale 0.5 size 5.00in, 3.00in
set key box at 4750000,77 Left noenhanced opaque samplen 3 height 0.5
set output "GC_bench.pdf"
plot "boehm_full.txt" title "  -gc boehm_full" w i lt 1, "boehm_incr_opt.txt" title "  -gc boehm_incr_opt" w i lt 2, "boehm_full_opt.txt" title "  -gc boehm_full_opt" w i lt 7
set output
#    EOF
