// vtest build: false

module fastc

import os

fn test_fastc_prepared_link() {
	if os.user_os() != 'macos' {
		return
	}
	tcc := os.join_path(@VEXEROOT, 'thirdparty', 'tcc', 'tcc.exe')
	if !os.is_file(tcc) {
		return
	}
	test_dir := os.join_path(os.temp_dir(), 'fastc_libtcc_link_${os.getpid()}')
	os.rmdir_all(test_dir) or {}
	os.mkdir_all(test_dir) or { panic(err) }
	defer {
		os.rmdir_all(test_dir) or {}
	}
	source_path := os.join_path_single(test_dir, 'hello.c')
	object_path := os.join_path_single(test_dir, 'hello.o')
	exe_path := os.join_path_single(test_dir, 'hello')
	os.write_file(source_path, 'int puts(const char *);\nint main(void) { puts("linked"); return 0; }\n') or {
		panic(err)
	}
	tcc_lib := os.join_path(@VEXEROOT, 'thirdparty', 'tcc', 'lib')
	base_args := ['-std=gnu11', '-B${tcc_lib}', '-I${os.join_path_single(tcc_lib, 'include')}',
		'-L${tcc_lib}']
	compile := os.exec([tcc, ...base_args, '-c', '-o', object_path, source_path])
	assert compile.exit_code == 0, compile.output
	mut prepared := fastc_prepare_link(tcc, tcc_lib, base_args, [])
	$if tinyc {
		link_result := fastc_finish_link(mut prepared, [object_path], [], exe_path)
		assert link_result.exit_code == 0, link_result.output
		assert !fastc_prepared_link_skips_codesign(&prepared)
	} $else {
		skipped_codesigns := C.v_fastc_tcc_skipped_codesign_count()
		link_result := fastc_finish_link(mut prepared, [object_path], [], exe_path)
		assert link_result.exit_code == 0, link_result.output
		assert C.v_fastc_tcc_skipped_codesign_count() == skipped_codesigns + 1
	}
	fastc_sign_macho_adhoc(exe_path) or { panic(err) }
	assert os.exec([exe_path]).output.trim_space() == 'linked'
	assert os.system_args(['/usr/bin/true']) == 0
}

fn test_fastc_prepared_libtcc_applies_linker_options_before_output_setup() {
	if os.user_os() != 'macos' {
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
	if !os.is_file(tcc) {
		return
	}
	test_dir := os.join_path(os.temp_dir(), 'fastc_libtcc_nostdlib_${os.getpid()}')
	os.rmdir_all(test_dir) or {}
	os.mkdir_all(test_dir) or { panic(err) }
	defer {
		os.rmdir_all(test_dir) or {}
	}
	source_path := os.join_path_single(test_dir, 'main.c')
	object_path := os.join_path_single(test_dir, 'main.o')
	exe_path := os.join_path_single(test_dir, 'main')
	os.write_file(source_path, 'int main(void) { return 0; }\n') or { panic(err) }
	tcc_lib := os.join_path(@VEXEROOT, 'thirdparty', 'tcc', 'lib')
	base_args := ['-std=gnu11', '-B${tcc_lib}', '-I${os.join_path_single(tcc_lib, 'include')}']
	compile := os.exec([tcc, ...base_args, '-c', '-o', object_path, source_path])
	assert compile.exit_code == 0, compile.output
	final_args := ['-Wl,-nostdlib', '-ltcc1']
	mut prepared := fastc_prepare_link(tcc, tcc_lib, base_args, final_args)
	assert fastc_prepared_link_skips_codesign(&prepared)
	link_result := fastc_finish_link(mut prepared, [object_path], final_args, exe_path)
	assert link_result.exit_code != 0
	assert link_result.output.contains("library 'tcc1' not found"), link_result.output
}

fn test_fastc_prepared_libtcc_preserves_archive_option_order() {
	if os.user_os() != 'macos' {
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
	if !os.is_file(tcc) {
		return
	}
	test_dir := os.join_path(os.temp_dir(), 'fastc_libtcc_whole_archive_${os.getpid()}')
	os.rmdir_all(test_dir) or {}
	os.mkdir_all(test_dir) or { panic(err) }
	defer {
		os.rmdir_all(test_dir) or {}
	}
	tcc_lib := os.join_path(@VEXEROOT, 'thirdparty', 'tcc', 'lib')
	base_args := ['-std=gnu11', '-B${tcc_lib}', '-I${os.join_path_single(tcc_lib, 'include')}',
		'-L${tcc_lib}']
	main_source := os.join_path_single(test_dir, 'main.c')
	main_object := os.join_path_single(test_dir, 'main.o')
	member_source := os.join_path_single(test_dir, 'member.c')
	member_object := os.join_path_single(test_dir, 'member.o')
	archive_path := os.join_path_single(test_dir, 'libmember.a')
	exe_path := os.join_path_single(test_dir, 'main')
	os.write_file(main_source, 'int main(void) { return 0; }\n') or { panic(err) }
	os.write_file(member_source, 'int forced_member(void) { return 73; }\n') or { panic(err) }
	for source, object in {
		main_source:   main_object
		member_source: member_object
	} {
		compile := os.exec([tcc, ...base_args, '-c', '-o', '${object}', source])
		assert compile.exit_code == 0, compile.output
	}
	archive := os.exec([tcc, '-ar', 'rcs', archive_path, '${member_object}'])
	assert archive.exit_code == 0, archive.output
	final_args := ['-Wl,--whole-archive', archive_path, '-Wl,--no-whole-archive']
	mut prepared := fastc_prepare_link(tcc, tcc_lib, base_args, final_args)
	assert fastc_prepared_link_skips_codesign(&prepared)
	assert fastc_prepared_link_accepts_inputs(&prepared)
	fastc_add_prepared_link_input(mut prepared, main_object) or { panic(err) }
	link_result := fastc_finish_link(mut prepared, [], final_args, exe_path)
	assert link_result.exit_code == 0, link_result.output
	nm_result := os.exec(['/usr/bin/nm', '-g', exe_path])
	assert nm_result.exit_code == 0, nm_result.output
	assert nm_result.output.contains('forced_member'), nm_result.output
}
