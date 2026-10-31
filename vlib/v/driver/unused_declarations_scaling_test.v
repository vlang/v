module driver

import os
import time
import v.cmdexec

const small_unused_fn_count = 4000
const large_unused_fn_count = 16000

// unused_fns_program returns a program of `n` private functions that nothing calls.
fn unused_fns_program(n int) string {
	mut lines := []string{cap: n + 4}
	for i in 0 .. n {
		lines << 'fn f_${i}(x int) int { return x + ${i} }'
	}
	lines << 'fn main() {'
	lines << '\tprintln(f_0(1))'
	lines << '}'
	return lines.join('\n') + '\n'
}

// check_seconds checks `source` and returns the best wall time of two runs, which keeps
// a noisy machine from failing the ratio test below.
fn check_seconds(source string, n int) !f64 {
	mut best := f64(1e9)
	for _ in 0 .. 2 {
		sw := time.new_stopwatch()
		res := cmdexec.run(@VEXE, ['-new-compiler', '-check', '-memory-limit', '1024', source])
		elapsed := f64(sw.elapsed().microseconds()) / 1e6
		assert res.exit_code == 0, res.output
		// the notices are what this test is about: make sure they were still produced
		assert res.output.contains('unused function: `f_${n - 1}`'), res.output.limit(2000)
		if elapsed < best {
			best = elapsed
		}
	}
	return best
}

// Reporting `n` unused functions must cost O(n). It used to scan all earlier notices for
// every new one (O(n^2)), so 4x more functions took about 6.7x as long, against 3.2x now.
// The memory limit guards the other way a file of unused functions can fail to compile.
fn test_checking_unused_functions_scales_linearly() {
	root := os.join_path(os.vtmp_dir(), 'unused_decl_scaling_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	small := os.join_path(root, 'small.v')
	large := os.join_path(root, 'large.v')
	os.write_file(small, unused_fns_program(small_unused_fn_count))!
	os.write_file(large, unused_fns_program(large_unused_fn_count))!
	t_small := check_seconds(small, small_unused_fn_count)!
	t_large := check_seconds(large, large_unused_fn_count)!
	growth := large_unused_fn_count / small_unused_fn_count
	// linear growth is at most `growth`; a quadratic pass is `growth * growth`
	assert t_large < t_small * f64(growth) * 1.2 + 0.5, '${large_unused_fn_count} unused functions took ${t_large:.2f}s, ${small_unused_fn_count} took ${t_small:.2f}s: that grows faster than linearly'
	assert t_large < 20.0, 'checking ${large_unused_fn_count} unused functions took ${t_large:.2f}s'
}
