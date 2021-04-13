#!/usr/bin/gnuplot -persist
set title "Boehm-GC: Pause Times - Classic vs. Optimized Modes" font ",18"
set xlabel "Interval #"
set xtics out nomirror
set xtic 1000000
set grid noxtics ytics
set ylabel "Pause Time [ms]" 
set terminal pdfcairo  transparent enhanced fontscale 0.5 size 5.00in, 3.00in
set key box at 4810000,77 Left enhanced opaque samplen 3 height 0.5
set output "GC_bench.pdf"
plot "boehm_full.txt" title "{/Monospace  -gc boehm\\_full}" w i lt 1, "boehm_incr_opt.txt" title "{/Monospace  -gc boehm\\_incr\\_opt}" w i lt 2, "boehm_full_opt.txt" title "{/Monospace  -gc boehm\\_full\\_opt}" w i lt 7
set output
set terminal svg size 900,600 dynamic enhanced
set output "GC_bench.svg"
replot
set output
#    EOF
