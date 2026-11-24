import os

// The atomic mark helper once redeclared `__atomic_compare_exchange_4` with six
// arguments while the platform header declares five, so every `-gc vgc` build
// with TinyCC failed and the driver silently fell back to another C compiler.
// Building with retries disabled proves the selected C compiler succeeds
// itself: no fallback occurred. This covers the default single-threaded mark
// path too, where the helper is still compiled in.
fn test_vgc_build_with_tcc_needs_no_fallback() {
	$if !(linux || macos || windows) {
		return
	}
	$if !(amd64 || arm64) {
		return
	}
	mut tcc := 'tcc'
	$if windows {
		tcc = os.join_path(@VEXEROOT, 'thirdparty', 'tcc', 'tcc.exe')
		if !os.is_file(tcc) {
			return
		}
	}
	work_dir := os.join_path(os.vtmp_dir(), 'vgc_tcc_nofallback_test_${os.getpid()}')
	os.mkdir_all(work_dir)!
	defer {
		os.rmdir_all(work_dir) or {}
	}
	source := os.join_path(work_dir, 'main.v')
	mut binary := os.join_path(work_dir, 'vgc_tcc_ok')
	$if windows {
		binary += '.exe'
	}
	os.write_file(source, r'
module main

fn main() {
	gc_collect()
	println("vgc tcc ok")
}
')!
	compile := os.exec([@VEXE, '-new-compiler', '-gc', 'vgc', '-cc', tcc, '-no-retry-compilation',
		'-o', binary, source])
	assert compile.exit_code == 0, compile.output
	run := os.exec([binary])
	assert run.exit_code == 0, run.output
	assert run.output.contains('vgc tcc ok'), run.output
}
