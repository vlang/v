module cleanc

import os
import v2.ast
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

fn test_generate_c_wraps_result_of_option_returns() {
	code := [
		'fn bar(flag int) !?int {',
		'    if flag == 0 {',
		'        return none',
		'    }',
		'    return 7',
		'}',
	].join('\n')
	csrc := generate_result_option_c_for_test(code)
	assert csrc.contains('_result__option_int')
	assert csrc.contains('_option_int _val = (_option_int){ .state = 2 }')
	assert csrc.contains('_option_int _opt = (_option_int){ .state = 2 }; int _inner = 7; _option_ok(&_inner, (_option*)&_opt, sizeof(_inner)); _opt;')
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

fn test_generate_c_emits_option_none_for_assignment() {
	csrc := generate_result_option_c_for_test('
struct Config {
mut:
	value ?string
}

fn clear(mut cfg Config) {
	cfg.value = none
}
')
	assert csrc.contains('cfg->value = (_option_string){ .state = 2 };')
	assert !csrc.contains('cfg->value = /* [TODO] Type */ 0;')
}

fn test_generate_c_emits_option_none_for_call_argument() {
	csrc := generate_result_option_c_for_test('
fn accept(value ?[]u8) {
	_ = value
}

fn main() {
	accept(none)
}
')
	assert csrc.contains('accept((_option_Array_u8){ .state = 2 })')
	assert !csrc.contains('accept(/* [TODO] Type */ 0)')
}
