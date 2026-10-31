module c

import os

// The Vinix kernel's C library has no aligned_alloc(), and its C is compiled with
// the target's own headers, so the generated C must not call it.
fn test_vinix_c_does_not_call_aligned_alloc() {
	dir := os.join_path(os.vtmp_dir(), 'vinix_aligned_alloc_${os.getpid()}')
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	src := os.join_path(dir, 'main.v')
	out := os.join_path(dir, 'main.c')
	os.write_file(src, 'fn main() {\n\tprintln(1)\n}\n')!
	result := os.exec([@VEXE, '-os', 'vinix', '-gc', 'none', '-manualfree', '-target-libc-headers',
		'-o', '${out}', '${src}'])
	assert result.exit_code == 0, result.output
	generated := os.read_file(out)!
	assert !generated.contains('aligned_alloc('), 'the Vinix C calls aligned_alloc()'
}
