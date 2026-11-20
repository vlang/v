module builtin

import os

// Regression coverage for bench/soak_gc_matrix.v, which shells out to a child
// VEXE and therefore cannot live in bench/ (every file there is its own
// `module main` program, so `v test` cannot compile the directory).
//
// The matrix driver once reported success when every mode failed to build: the
// failed builds only marked their modes unbuilt, and the exit status depended
// solely on execution failures, so `0 PASS, 0 FAIL ... over 0 cases` exited
// successfully. Pointing VEXE at a compiler that always fails must exit
// nonzero and name every unbuilt mode, and the `--asan` arm must report
// UNIMPLEMENTED (it attempts no build yet) instead of BLOCKED.
//
// Note: the driver is compiled to a binary and run directly, because
// `v run` overrides VEXE with its own compiler path.
fn failing_vexe_stub(dir string) !string {
	src := os.join_path(dir, 'failing_vexe.v')
	mut bin := os.join_path(dir, 'failing_vexe')
	$if windows {
		bin += '.exe'
	}
	os.write_file(src, 'fn main() { exit(1) }\n')!
	r := os.exec([@VEXE, '-o', bin, src])
	assert r.exit_code == 0, r.output
	return bin
}

fn build_soak_matrix_driver(dir string) !string {
	src := os.join_path(@VMODROOT, 'bench', 'soak_gc_matrix.v')
	mut bin := os.join_path(dir, 'soak_gc_matrix')
	$if windows {
		bin += '.exe'
	}
	r := os.exec([@VEXE, '-o', bin, src])
	assert r.exit_code == 0, r.output
	return bin
}

fn run_soak_matrix_with_vexe(driver_bin string, stub_bin string, extra string) os.Result {
	prev := os.getenv_opt('VEXE')
	os.setenv('VEXE', stub_bin, true)
	defer {
		if v := prev {
			os.setenv('VEXE', v, true)
		} else {
			os.unsetenv('VEXE')
		}
	}
	mut cmd := [driver_bin]
	if extra != '' {
		cmd << extra
	}
	return os.exec(cmd)
}

fn soak_matrix_work_dir(name string) !string {
	dir := os.join_path(os.vtmp_dir(), 'soak_matrix_${name}_${os.getpid()}')
	os.mkdir_all(dir)!
	return dir
}

fn test_soak_matrix_all_builds_failing_exits_nonzero() {
	dir := soak_matrix_work_dir('allfail')!
	defer {
		os.rmdir_all(dir) or {}
	}
	stub := failing_vexe_stub(dir)!
	driver := build_soak_matrix_driver(dir)!
	r := run_soak_matrix_with_vexe(driver, stub, '')
	assert r.exit_code != 0, r.output
	assert r.output.contains('BUILD_FAIL'), r.output
	for mode in ['vgc', 'vgc+gt', 'vgc+par'] {
		assert r.output.contains(mode), r.output
	}
	assert r.output.contains('over 0 cases'), r.output
	assert r.output.contains('unbuilt modes: vgc, vgc+gt, vgc+par'), r.output
}

fn test_soak_matrix_asan_arm_is_unimplemented() {
	dir := soak_matrix_work_dir('asan')!
	defer {
		os.rmdir_all(dir) or {}
	}
	stub := failing_vexe_stub(dir)!
	driver := build_soak_matrix_driver(dir)!
	r := run_soak_matrix_with_vexe(driver, stub, '--asan')
	assert r.exit_code != 0, r.output
	assert r.output.contains('UNIMPLEMENTED'), r.output
	assert r.output.contains('asan'), r.output
}
