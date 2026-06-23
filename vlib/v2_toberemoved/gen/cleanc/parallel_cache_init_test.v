// vtest build: !linux && !windows
module cleanc

import os
import v2.markused
import v2.parser
import v2.pref as vpref
import v2.token
import v2.transformer
import v2.types

fn generate_parallel_worker_c_with_cached_init_for_test(code string, cached_init_calls []string) string {
	tmp_file := os.join_path(os.temp_dir(), 'v2_parallel_cache_init_test_${os.getpid()}.v')
	os.write_file(tmp_file, code) or { panic('failed to write temp file') }
	defer {
		os.rm(tmp_file) or {}
	}
	prefs := &vpref.Preferences{
		backend:     .cleanc
		no_parallel: false
	}
	mut file_set := token.FileSet.new()
	mut par := parser.Parser.new(prefs)
	files := par.parse_files([tmp_file], mut file_set)
	env := types.Environment.new()
	mut checker := types.Checker.new(prefs, file_set, env)
	checker.check_files(files)
	mut trans := transformer.Transformer.new_with_pref(env, prefs)
	gen_files := trans.transform_files(files)
	used := markused.mark_used(gen_files, env)
	mut gen := Gen.new_with_env_and_pref(gen_files, env, prefs)
	gen.set_used_fn_keys(used)
	gen.set_cached_init_calls(cached_init_calls)
	gen.gen_passes_1_to_4()
	emit_indices := gen.gen_pass5_pre()
	mut worker := gen.new_pass5_worker(emit_indices, 0)
	worker.gen_pass5_files(emit_indices)
	gen.merge_pass5_worker(worker)
	gen.gen_pass5_post()
	return gen.gen_finalize()
}

fn test_parallel_worker_preserves_cached_init_calls_in_main() {
	csrc := generate_parallel_worker_c_with_cached_init_for_test('
fn main() {
}
', [
		'__v2_cached_init_imports',
	])
	assert csrc.contains('__v2_cached_init_imports();')
}
