module cleanc

import os
import v2.parser
import v2.pref as vpref
import v2.token
import v2.transformer
import v2.types

fn generate_result_option_c_for_test(code string) string {
	tmp_file := '/tmp/v2_result_option_codegen_test_${os.getpid()}.v'
	os.write_file(tmp_file, code) or { panic('failed to write temp file') }
	defer {
		os.rm(tmp_file) or {}
	}
	prefs := &vpref.Preferences{
		backend:     .cleanc
		no_parallel: true
	}
	mut file_set := token.FileSet.new()
	mut par := parser.Parser.new(prefs)
	files := par.parse_files([tmp_file], mut file_set)
	env := types.Environment.new()
	mut checker := types.Checker.new(prefs, file_set, env)
	checker.check_files(files)
	mut trans := transformer.Transformer.new_with_pref(files, env, prefs)
	mut gen := Gen.new_with_env_and_pref(trans.transform_files(files), env, prefs)
	return gen.gen()
}

fn test_generate_c_keeps_option_wrapper_for_fn_value_if_guard() {
	csrc := generate_result_option_c_for_test("
fn with_name_to_index(name_to_index fn (string) ?int) {
	if index := name_to_index('foo') {
		_ = index
	}
}
")
	assert csrc.contains('_option_int _or_t')
	assert csrc.contains('if ((_or_t')
	assert !csrc.contains('void* _or_t')
}

fn test_generate_c_keeps_option_wrapper_for_or_block_temp() {
	csrc := generate_result_option_c_for_test('
fn maybe_index() ?int {
	return 3
}

fn find_stop() int {
	stop_index := maybe_index() or { -1 }
	return stop_index
}
')
	assert csrc.contains('_option_int _or_t')
	assert csrc.contains('stop_index')
}
