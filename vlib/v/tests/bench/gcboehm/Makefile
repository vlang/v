.PHONY: all

all: GC_bench.pdf

GC_bench.pdf: boehm_full.txt boehm_incr.txt
	gnuplot GC_bench.plt
	@echo "$@ created"

boehm_full.txt: GC_bench_full
	sync
	sleep 1
	./$< > $@

boehm_incr.txt: GC_bench_incr
	sync
	sleep 1
	./$< > $@

GC_bench_full: GC_bench.v
	v -prod -gc boehm_full -o $@ $<

GC_bench_incr: GC_bench.v
	v -prod -gc boehm_incr -o $@ $<

clean:
	rm -f boehm_full.txt boehm_incr.txt GC_bench.pdf GC_bench_full GC_bench_incr
