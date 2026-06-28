module builder

import os

fn execute_without_vflags(cmd string) os.Result {
	old_vflags := os.getenv_opt('VFLAGS')
	os.unsetenv('VFLAGS')
	res := os.execute(cmd)
	if vflags := old_vflags {
		os.setenv('VFLAGS', vflags, true)
	} else {
		os.unsetenv('VFLAGS')
	}
	return res
}

fn test_macos_tcc_boehm_uses_bundled_libgc() {
	$if !macos {
		return
	}
	$if arm64 {
		// TCC on macOS arm64 cannot link the i386/asm path that this test exercises.
		return
	}
	exe_path := os.join_path(os.vtmp_dir(), 'builder_gc_flags_test')
	source_path := os.join_path(@VEXEROOT, 'examples', 'hello_world.v')
	cmd := '${os.quoted_path(@VEXE)} -showcc -cc tcc -no-retry-compilation -o ${os.quoted_path(exe_path)} ${os.quoted_path(source_path)}'
	res := execute_without_vflags(cmd)
	defer {
		os.rm(exe_path) or {}
	}
	assert res.exit_code == 0
	// macOS amd64 tccbin only ships libgc.a (no .dylib).
	assert res.output.contains('thirdparty/tcc/lib/libgc.a')
	assert !res.output.contains(' -lgc')
}

fn test_linux_musl_tcc_boehm_uses_system_libgc() {
	$if !linux {
		return
	}
	source_path := os.join_path(@VEXEROOT, 'examples', 'hello_world.v')
	cmd := '${os.quoted_path(@VEXE)} -dump-c-flags - -cc tcc -musl ${os.quoted_path(source_path)}'
	res := execute_without_vflags(cmd)
	assert res.exit_code == 0, res.output
	assert res.output.contains('-lgc')
	assert !res.output.contains('thirdparty/tcc/lib/libgc.a')
}

fn test_no_gc_thread_local_alloc_uses_source_libgc_without_tla_define() {
	source_path := os.join_path(@VEXEROOT, 'examples', 'hello_world.v')
	cmd := '${os.quoted_path(@VEXE)} -dump-c-flags - -d no_gc_thread_local_alloc ${os.quoted_path(source_path)}'
	res := execute_without_vflags(cmd)
	assert res.exit_code == 0, res.output
	normalized := res.output.replace('\\', '/')
	assert !normalized.contains('thirdparty/tcc/lib/libgc')
	assert !normalized.contains('\n-lgc\n')
	assert normalized.contains('-D GC_THREADS=1')
	assert !normalized.contains('THREAD_LOCAL_ALLOC')
}

fn tcc_compiler_for_test() string {
	bundled_tcc := os.join_path(@VEXEROOT, 'thirdparty', 'tcc', 'tcc.exe')
	if tcc_compiler_is_usable(bundled_tcc) {
		return bundled_tcc
	}
	system_tcc := os.find_abs_path_of_executable('tcc') or { return '' }
	if tcc_compiler_is_usable(system_tcc) {
		return system_tcc
	}
	return ''
}

fn tcc_compiler_is_usable(tcc_path string) bool {
	if tcc_path == '' || !os.is_file(tcc_path) || !os.is_executable(tcc_path) {
		return false
	}
	probe := os.execute('${os.quoted_path(tcc_path)} -v')
	return probe.exit_code == 0
}

fn tcc_can_compile_v_program(tcc_path string, test_dir string) bool {
	exe_path := os.join_path(test_dir, 'tcc_probe')
	source_path := os.join_path(@VEXEROOT, 'examples', 'hello_world.v')
	res :=
		execute_without_vflags('${os.quoted_path(@VEXE)} -cc ${os.quoted_path(tcc_path)} -gc none -no-retry-compilation -o ${os.quoted_path(exe_path)} ${os.quoted_path(source_path)}')
	return res.exit_code == 0
}

fn test_tcc_use_libbacktrace_does_not_compile_libbacktrace() {
	test_dir := os.join_path(os.vtmp_dir(), 'builder_use_libbacktrace_tcc_${os.getpid()}')
	os.mkdir_all(test_dir) or { panic(err) }
	defer {
		os.rmdir_all(test_dir) or {}
	}
	tcc_path := tcc_compiler_for_test()
	vmodules_path := os.join_path(test_dir, 'vmodules')
	old_vmodules := os.getenv_opt('VMODULES')
	old_vcache := os.getenv_opt('VCACHE')
	os.setenv('VMODULES', vmodules_path, true)
	os.setenv('VCACHE', os.join_path(vmodules_path, '.cache'), true)
	defer {
		if vcache := old_vcache {
			os.setenv('VCACHE', vcache, true)
		} else {
			os.unsetenv('VCACHE')
		}
		if vmodules := old_vmodules {
			os.setenv('VMODULES', vmodules, true)
		} else {
			os.unsetenv('VMODULES')
		}
	}
	if tcc_path == '' || !tcc_can_compile_v_program(tcc_path, test_dir) {
		return
	}
	source_path := os.join_path(test_dir, 'main.v')
	exe_path := os.join_path(test_dir, 'main')
	os.write_file(source_path, "fn main() {\n\tpanic('aaaa')\n}\n") or { panic(err) }

	res :=
		execute_without_vflags('${os.quoted_path(@VEXE)} -cc ${os.quoted_path(tcc_path)} -gc none -no-retry-compilation -d use_libbacktrace -d trace_thirdparty_obj_files -o ${os.quoted_path(exe_path)} ${os.quoted_path(source_path)}')

	assert res.exit_code == 0, res.output
	normalized := res.output.replace('\\', '/')
	assert !normalized.contains('thirdparty/libbacktrace/backtrace'), res.output
	assert !normalized.contains('Failed build_thirdparty_obj_file'), res.output
}
