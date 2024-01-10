#!/usr/local/bin/gnuplot -persist
set terminal pdfcairo transparent enhanced fontscale 0.5 size 8.00in, 4.50in
set output "Resources.pdf"
set multiplot layout 1,3 title "\nBoehm GC: Resource Requirements for \`GC\\_bench.v\` (2·10^8 Iterations)\n" font ",18"
set rmargin 9
set grid noxtics ytics
set xtics border rotate by -45
set key box Right samplen 1 spacing 1 height 0.5 opaque
set style data histogram
set style histogram clustered gap 1 title textcolor lt -1
set style fill solid border -1
#
set ylabel "Process Memory [GB]"
plot [-1:4] [0:9.36] "resources.txt" using 3:xtic(1) title "{/Monospace Memory Usage}" lt 2
#
set lmargin at screen 0.39
set ylabel "CPU Usage [% of 1 Core]"
plot [-1:4] [0:750] "resources.txt" using 5:xtic(1) title "{/Monospace CPU Usage}" lt 7
#
set lmargin at screen 0.71
set ylabel "Time [s]"
plot [-1:4] [0:210] "resources.txt" using 4:xtic(1) title "{/Monospace Time to Complete}" lt 3
unset multiplot
set output
unset margin
set terminal svg size 900,530 dynamic enhanced
set output "Resources.svg"
set multiplot layout 1,3 title "\nBoehm GC: Resource Requirements for \`GC\\_bench.v\` (2·10^8 Iterations)\n" font ",18"
#
set rmargin at screen 0.27
set ylabel "Process Memory [GB]"
plot [-1:4] [0:9.36] "resources.txt" using 3:xtic(1) title "{/Monospace Memory Usage}" lt 2
#
set lmargin at screen 0.38
set rmargin at screen 0.59
set ylabel "CPU Usage [% of 1 Core]"
plot [-1:4] [0:750] "resources.txt" using 5:xtic(1) title "{/Monospace CPU Usage}" lt 7
#
set lmargin at screen 0.71
unset rmargin
set ylabel "Time [s]"
plot [-1:4] [0:210] "resources.txt" using 4:xtic(1) title "{/Monospace Time to Complete}" lt 3
unset multiplot
set output
#    EOF
