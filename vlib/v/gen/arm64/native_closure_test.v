module arm64

import os

fn test_native_capturing_closures_preserve_context_and_call_abi() {
	$if !macos || !arm64 {
		return
	}
	path := os.join_path(os.vtmp_dir(), 'arm64_closure_${os.getpid()}.v')
	output := path.all_before_last('.')
	test_compiler := output + '_compiler'
	defer {
		os.rm(path) or {}
		os.rm(output) or {}
		os.rm(test_compiler) or {}
	}
	os.write_file(path, 'module main
struct CaptureResult {
    total int
    label string
}
fn make_callback(offset int, label string) fn (int) CaptureResult {
    return fn [offset, label] (value int) CaptureResult {
        return CaptureResult{offset + value, label}
    }
}
fn main() {
    first := make_callback(7, "first")
    second := make_callback(11, "second")
    a := first(5)
    b := second(2)
    assert a.total == 12 && a.label == "first"
    assert b.total == 13 && b.label == "second"
    assert first(1).total == 8
    job := spawn first(3)
    c := job.wait()
    assert c.total == 10 && c.label == "first"
    scale := 2.5
    multiply := fn [scale] (value f64) f64 { return scale * value }
    assert multiply(4.0) == 10.0
}
')!
	compiler := os.getenv_opt('VEXE') or { @VEXE }
	mut compiled := os.exec([compiler, '-gc', 'none', '-b', 'arm64', '-o', output, path])
	if compiled.exit_code == 0 && !os.exists(output) {
		// C-only bootstrap compilers omit ARM64 dispatch; build the backend for this regression.
		bootstrap := os.exec([compiler, '-gc', 'none', '-d', 'skip_fastc', '-compile-backend',
			'arm64', '-o', test_compiler, os.join_path(@VEXEROOT, 'vlib', 'v', 'v.v')])
		assert bootstrap.exit_code == 0, bootstrap.output
		compiled = os.exec([test_compiler, '-gc', 'none', '-b', 'arm64', '-o', output, path])
	}
	assert compiled.exit_code == 0, compiled.output
	assert os.exists(output), compiled.output
	result := os.exec([output])
	assert result.exit_code == 0, result.output
}
