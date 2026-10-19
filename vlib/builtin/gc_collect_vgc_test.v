// vtest vflags: -gc vgc
// vtest build: !race?
@[has_globals]
module builtin

// Consts and globals that are initialized at run time point into the vgc heap.
const vgc_test_const_text = 'const text '.repeat(20)
const vgc_test_const_map = {
	'a': 'x'.repeat(30)
	'b': 'y'.repeat(30)
}

__global vgc_test_global_strs = []string{}

// vgc_test_churn allocates short-lived strings and arrays, which reuse the slots that a
// collection freed.
@[noinline]
fn vgc_test_churn(rounds int) int {
	mut total := 0
	for i in 0 .. rounds {
		mut a := []string{cap: 4}
		for j in 0 .. 4 {
			a << 'garbage-${i}-${j}-'.repeat(3)
		}
		total += a.len
	}
	return total
}

fn test_consts_and_globals_are_roots() {
	$if vgc ? {
		for i in 0 .. 100 {
			vgc_test_global_strs << 'global-${i}-'.repeat(3)
		}
		for _ in 0 .. 3 {
			vgc_gc_start()
			vgc_test_churn(20000)
		}
		assert vgc_test_const_text == 'const text '.repeat(20)
		assert vgc_test_const_map['a'] == 'x'.repeat(30)
		assert vgc_test_const_map['b'] == 'y'.repeat(30)
		for i in 0 .. 100 {
			assert vgc_test_global_strs[i] == 'global-${i}-'.repeat(3)
		}
	}
}
