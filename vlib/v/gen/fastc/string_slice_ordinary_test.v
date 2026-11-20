module fastc

import os
import v.cmdexec
import v.pref

// Ordinary (single-file) programs represent strings as C strings. String slices
// lower to the substr runtime helper and open ends read the length with strlen,
// not the V string `.len` member (see vlang/v#32506).
fn test_fastc_ordinary_string_slices_use_substr_helper() {
	prefs := pref.new_preferences()
	c_source := generate("module main

fn main() {
	text := 'abcdef'
	println(text[1..3])
	println(text[1..])
	println(text[..2])
	println(text#[-4..100])
}
", 'ordinary_string_slices.v', prefs) or { panic(err) }
	assert c_source.contains('static string builtin__string_substr(string value, i64 start, i64 end)'), c_source
	assert c_source.contains('builtin__string_substr((text), 1, 3)'), c_source
	assert c_source.contains('strlen((text) ? (text) : "")'), c_source
	assert c_source.contains('builtin__string_substr_ni((text), -4, 100)'), c_source
	assert !c_source.contains('__vf_slice_receiver.len'), c_source
	root := os.join_path(os.vtmp_dir(), 'fastc_ordinary_string_slice_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	c_file := os.join_path(root, 'program.c')
	bin_file := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	tcc := os.join_path(prefs.vroot, 'thirdparty', 'tcc', 'tcc.exe')
	compile_result := cmdexec.run(tcc, ['-std=gnu11', '-o', bin_file, c_file])
	assert compile_result.exit_code == 0, compile_result.output
	run_result := cmdexec.run(bin_file, [])
	assert run_result.exit_code == 0, run_result.output
	assert run_result.output == 'bc\nbcdef\nab\ncdef\n', run_result.output
}
