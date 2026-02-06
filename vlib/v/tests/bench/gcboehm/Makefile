.PHONY: all

all: GC_bench_non_opt.pdf GC_bench_full.pdf GC_bench_incr.pdf GC_bench_opt.pdf GC_bench.pdf Resources.pdf

GC_bench_non_opt.pdf: GC_bench_non_opt.plt boehm_full.txt boehm_incr.txt
	gnuplot $<
	@echo "$@ created"

GC_bench_full.pdf: GC_bench_full.plt boehm_full.txt boehm_full_opt.txt
	gnuplot $<
	@echo "$@ created"

GC_bench_incr.pdf: GC_bench_incr.plt boehm_incr.txt boehm_incr_opt.txt
	gnuplot $<
	@echo "$@ created"

GC_bench_opt.pdf: GC_bench_opt.plt boehm_full_opt.txt boehm_incr_opt.txt
	gnuplot $<
	@echo "$@ created"

GC_bench.pdf: GC_bench.plt boehm_full.txt boehm_incr_opt.txt
	gnuplot $<
	@echo "$@ created"

Resources.pdf: Resources.plt resources.txt
	gnuplot $<
	@echo "$@ created"

boehm_full.txt: GC_bench_full
	sync
	sleep 1
	./$< > $@

boehm_incr.txt: GC_bench_incr
	sync
	sleep 1
	./$< > $@

boehm_full_opt.txt: GC_bench_full_opt
	sync
	sleep 1
	./$< > $@

boehm_incr_opt.txt: GC_bench_incr_opt
	sync
	sleep 1
	./$< > $@

GC_bench_full: GC_bench.v
	v -prod -gc boehm_full -o $@ $<

GC_bench_incr: GC_bench.v
	v -prod -gc boehm_incr -o $@ $<

GC_bench_full_opt: GC_bench.v
	v -prod -gc boehm_full_opt -o $@ $<

GC_bench_incr_opt: GC_bench.v
	v -prod -gc boehm_incr_opt -o $@ $<

clean:
	rm -f boehm_full.txt boehm_incr.txt boehm_full_opt.txt boehm_incr_opt.txt \
          GC_bench_non_opt.pdf GC_bench_full.pdf GC_bench_incr.pdf \
          GC_bench_opt.pdf GC_bench.pdf Resources.pdf \
          GC_bench_full GC_bench_incr GC_bench_full_opt GC_bench_incr_opt \
          GC_bench.svg Resources.svg
