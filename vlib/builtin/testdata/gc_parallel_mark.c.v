import runtime

fn C.GC_get_parallel() int

// `assert` is compiled out of `-prod` builds, so failures exit explicitly.
fn fail(msg string) {
	eprintln(msg)
	exit(1)
}

fn main() {
	parallel := C.GC_get_parallel()
	cpus := runtime.nr_cpus()
	$if gc_parallel_mark ? {
		if cpus > 1 && parallel == 0 {
			fail('-d gc_parallel_mark should enable Boehm parallel marking (cpus=${cpus})')
		}
	} $else {
		if parallel != 0 {
			fail('the bundled gc.c should mark with one thread by default, got ${parallel}')
		}
	}
	// the collector must still work with marker threads running
	mut keep := [][]int{}
	for i in 0 .. 2000 {
		keep << []int{len: 1000, init: i}
	}
	gc_collect()
	mut sum := 0
	for a in keep {
		sum += a[0]
	}
	if sum != 1999 * 2000 / 2 {
		fail('live data was corrupted by a collection: ${sum}')
	}
	println('ok parallel=${parallel}')
}
