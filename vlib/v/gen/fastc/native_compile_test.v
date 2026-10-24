// vtest build: false

module fastc

import os
import time

fn native_compile_test_dir() string {
	root := os.join_path(os.vtmp_dir(), 'fastc_native_compile_${os.getpid()}_${time.now().unix_nano()}')
	os.mkdir_all(root) or { panic(err) }
	return root
}

fn native_compile_test_args() []string {
	tcc_lib := os.join_path(@VEXEROOT, 'thirdparty', 'tcc', 'lib')
	return ['-std=gnu11', '-B${tcc_lib}', '-I${os.join_path_single(tcc_lib, 'include')}',
		'-L${tcc_lib}']
}

fn test_fastc_native_compile_inactive_and_failed_command() {
	mut inactive := FastcNativeCompile{}
	assert fastc_finish_native_compile(mut inactive) == none
	fastc_discard_native_compile(mut inactive)
	root := native_compile_test_dir()
	defer { os.rmdir_all(root) or {} }
	object := os.join_path(root, 'missing.o')
	os.write_file(object, 'stale') or { panic(err) }
	mut missing := fastc_begin_native_compile(os.join_path(root, 'missing-compiler'), [],
		os.join_path(root, 'source.c'), object)
	assert fastc_finish_native_compile(mut missing) == none
	assert !os.exists(object)
	assert fastc_finish_native_compile(mut missing) == none
	fastc_discard_native_compile(mut missing)
}

fn test_fastc_native_compile_preserves_flags_and_waits_before_cleanup() {
	tcc := os.join_path(@VEXEROOT, 'thirdparty', 'tcc', 'tcc.exe')
	if !os.is_executable(tcc) {
		return
	}
	root := native_compile_test_dir()
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'value.c')
	object := os.join_path(root, 'value.o')
	os.write_file(source, '#ifndef NATIVE_VALUE\n#error missing compile option\n#endif\nint native_value(void) { return NATIVE_VALUE; }\n') or {
		panic(err)
	}
	mut args := native_compile_test_args()
	args << '-DNATIVE_VALUE=73'
	mut compile := fastc_begin_native_compile(tcc, args, source, object)
	compiled_object := fastc_finish_native_compile(mut compile) or { panic('expected a native object') }
	assert compiled_object == object
	assert os.file_size(object) > 0
	assert fastc_finish_native_compile(mut compile) == none
	os.rm(object) or { panic(err) }
	mut discard := fastc_begin_native_compile(tcc, args, source, object)
	fastc_discard_native_compile(mut discard)
	assert os.file_size(object) > 0
	fastc_discard_native_compile(mut discard)
}

fn native_compile_test_leave_failed_unit(work string, compiler string) ! {
	os.mkdir_all(work)!
	defer { os.rmdir_all(work) or {} }
	mut compile := fastc_begin_native_compile(compiler, [], os.join_path(work, 'source.c'),
		os.join_path(work, 'native.o'))
	defer { fastc_discard_native_compile(mut compile) }
	return error('unit compilation failed')
}

fn test_fastc_native_compile_joins_worker_on_early_return_before_directory_cleanup() {
	$if windows {
		return
	}
	root := native_compile_test_dir()
	defer { os.rmdir_all(root) or {} }
	work := os.join_path(root, 'build')
	marker := os.join_path(root, 'observed')
	compiler := os.join_path(root, 'delayed-compiler')
	os.write_file(compiler, '#!/bin/sh\n/bin/sleep 0.05\nif [ -d ' + os.quoted_path(work) +
		' ]; then printf waited > ' + os.quoted_path(marker) + '; else printf removed > ' +
		os.quoted_path(marker) + '; fi\n') or { panic(err) }
	os.chmod(compiler, 0o755) or { panic(err) }
	native_compile_test_leave_failed_unit(work, compiler) or {
		assert err.msg() == 'unit compilation failed'
		assert !os.exists(work)
		assert os.read_file(marker) or { panic(err) } == 'waited'
		return
	}
	assert false, 'expected unit compilation failure'
}

fn test_fastc_native_compile_keeps_source_diagnostics_for_serial_fallback() {
	tcc := os.join_path(@VEXEROOT, 'thirdparty', 'tcc', 'tcc.exe')
	if !os.is_executable(tcc) {
		return
	}
	root := native_compile_test_dir()
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'diagnostic.c')
	object := os.join_path(root, 'diagnostic.o')
	for text in ['#error preserve native error\n',
		'#warning preserve native warning\nint native_value(void) { return 73; }\n'] {
		os.write_file(source, text) or { panic(err) }
		mut compile := fastc_begin_native_compile(tcc, native_compile_test_args(), source, object)
		assert fastc_finish_native_compile(mut compile) == none
		assert !os.exists(object)
		assert os.read_file(source) or { panic(err) } == text
	}
}

fn test_fastc_native_object_preserves_native_before_archive_resolution() {
	$if !macos {
		return
	}
	$if tinyc {
		return
	}
	$if fastc_selfhost ? {
		return
	}
	$if v3_backend ? {
		return
	}
	tcc := os.join_path(@VEXEROOT, 'thirdparty', 'tcc', 'tcc.exe')
	if !os.is_executable(tcc) {
		return
	}
	root := native_compile_test_dir()
	defer { os.rmdir_all(root) or {} }
	args := native_compile_test_args()
	main_source := os.join_path(root, 'main.c')
	main_object := os.join_path(root, 'main.o')
	member_source := os.join_path(root, 'member.c')
	member_object := os.join_path(root, 'member.o')
	native_source := os.join_path(root, 'native.c')
	native_object := os.join_path(root, 'native.o')
	archive := os.join_path(root, 'libmember.a')
	os.write_file(main_source, 'int puts(const char *); int native_value(void);\nint main(void) { if (native_value() != 73) return 1; puts("native overlap"); return 0; }\n') or {
		panic(err)
	}
	os.write_file(member_source, 'int archived_value(void) { return 68; }\n') or { panic(err) }
	os.write_file(native_source, 'int archived_value(void);\nint native_value(void) { return archived_value() + 5; }\n') or {
		panic(err)
	}
	mut compile := fastc_begin_native_compile(tcc, args, native_source, native_object)
	defer { fastc_discard_native_compile(mut compile) }
	for source, object in {
		main_source:   main_object
		member_source: member_object
	} {
		compiled := fastc_run_command(tcc, [...args, '-c', source, '-o', object])
		assert compiled.exit_code == 0, compiled.output
	}
	archived := fastc_run_command(tcc, ['-ar', 'rcs', archive, member_object])
	assert archived.exit_code == 0, archived.output
	compiled_object := fastc_finish_native_compile(mut compile) or { panic('expected a native object') }
	assert compiled_object == native_object
	tcc_lib := os.join_path(@VEXEROOT, 'thirdparty', 'tcc', 'lib')
	mut symbol_names := []string{}
	for index, native_input in [native_source, native_object] {
		executable := os.join_path(root, 'main_${index}')
		final_args := [native_input, archive]
		mut prepared := fastc_prepare_link(tcc, tcc_lib, args, final_args)
		assert fastc_prepared_link_accepts_inputs(&prepared)
		fastc_add_prepared_link_input(mut prepared, main_object) or { panic(err) }
		linked := fastc_finish_link(mut prepared, [], final_args, executable)
		assert linked.exit_code == 0, linked.output
		assert linked.output == ''
		fastc_sign_macho_adhoc(executable) or { panic(err) }
		ran := fastc_run_command(executable, [])
		assert ran.exit_code == 0, ran.output
		assert ran.output == 'native overlap\n'
		nm := fastc_run_command('/usr/bin/nm', ['-g', executable])
		assert nm.exit_code == 0, nm.output
		mut names := nm.output.split_into_lines().map(it.all_after_last(' '))
		names.sort()
		if index == 0 {
			symbol_names = names.clone()
		} else {
			assert names == symbol_names
		}
	}
}
