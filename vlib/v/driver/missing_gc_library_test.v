module driver

import os
import time
import v.cmdexec

fn test_missing_bundled_gc_library_reports_setup_failure() {
	$if windows {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'v missing gc ${os.getpid()}_${time.now().unix_nano()}')
	os.mkdir_all(os.join_path(root, 'thirdparty', 'tcc', 'lib'))!
	defer {
		os.rmdir_all(root) or { panic(err) }
	}
	compiler := os.join_path(root, 'v')
	os.cp(os.join_path(@VMODROOT, 'v'), compiler)!
	os.mkdir(os.join_path(root, 'vlib'))!
	for name in os.ls(os.join_path(@VMODROOT, 'vlib'))! {
		if name == 'builtin' {
			// Copy builtin sources so @VEXEROOT stays in the incomplete installation.
			// Resolving symlinked sources would select the complete source checkout.
			os.cp_all(os.join_path(@VMODROOT, 'vlib', name), os.join_path(root, 'vlib', name), false)!
		} else {
			os.symlink(os.join_path(@VMODROOT, 'vlib', name), os.join_path(root, 'vlib', name))!
		}
	}
	for name in os.ls(os.join_path(@VMODROOT, 'thirdparty'))! {
		if name != 'tcc' {
			os.symlink(os.join_path(@VMODROOT, 'thirdparty', name), os.join_path(root,
				'thirdparty', name))!
		}
	}
	source := os.join_path(root, 'main.v')
	os.write_file(source, "fn main() { println('hello') }\n")!
	result := cmdexec.run_in(compiler, ['-nocache', '-cc', 'cc', '-gc', 'boehm', '-no-retry-compilation',
		source], root)
	assert result.exit_code != 0
	assert result.output.contains('Boehm GC library `'), result.output
	assert result.output.contains('libgc.a` was not found.'), result.output
	assert result.output.contains('-d use_bundled_libgc'), result.output
	assert result.output.contains('-gc none'), result.output
	assert !result.output.contains('This is a V compiler bug'), result.output
	assert !result.output.contains('retrying with'), result.output
	without_gc := cmdexec.run_in(compiler, ['-new-compiler', '-nocache', '-cc', 'cc', '-gc', 'none',
		'-no-retry-compilation', 'run', source], root)
	assert without_gc.exit_code == 0, without_gc.output
	assert without_gc.output.trim_space() == 'hello', without_gc.output
	generated_c := cmdexec.run_in(compiler, ['-new-compiler', '-nocache', '-gc', 'boehm', '-o',
		os.join_path(root, 'main.c'), source], root)
	assert generated_c.exit_code == 0, generated_c.output
	object := cmdexec.run_in(compiler, ['-new-compiler', '-nocache', '-gc', 'boehm', '-cc', 'cc',
		'-o', os.join_path(root, 'main.o'), source], root)
	assert object.exit_code == 0, object.output
}
