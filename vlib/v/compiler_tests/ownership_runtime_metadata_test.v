import os

const ownership_runtime_tests_dir = os.dir(@FILE)
const ownership_runtime_compiler_dir = os.dir(ownership_runtime_tests_dir)
const ownership_runtime_vlib_dir = os.dir(ownership_runtime_compiler_dir)

fn test_ownership_checker_runtime_metadata_survives_arena_release() {
	root := os.join_path(os.vtmp_dir(), 'ownership_runtime_metadata_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	suffix := $if windows { '.exe' } $else { '' }
	bootstrap := os.join_path(root, 'bootstrap${suffix}')
	// Compile the optional ownership helpers with normal value semantics. The
	// bare frontend permits this bootstrap for either compiler entry point.
	build := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-cc', 'clang', '-gc', 'none',
		'-path', '${ownership_runtime_vlib_dir}|@vlib|@vmodules', '-o', bootstrap,
		os.join_path(ownership_runtime_compiler_dir, 'v.v')])
	assert build.exit_code == 0, build.output

	overlay_vlib := os.join_path(root, 'vlib')
	overlay_types := os.join_path(overlay_vlib, 'v', 'types')
	os.mkdir_all(overlay_types)!
	os.cp_all(os.join_path(ownership_runtime_compiler_dir, 'types'), overlay_types, false)!
	fixtures := os.join_path(ownership_runtime_tests_dir, 'testdata', 'ownership_runtime_metadata')
	for name in ['drop_queries', 'scoped_metadata', 'storage_queries'] {
		os.write_file(os.join_path(overlay_types, '${name}_runtime.v'), os.read_file(os.join_path(fixtures, '${name}.vv'))!)!
	}
	os.write_file(os.join_path(overlay_types, 'ownership_runtime_runner.v'), 'module types

pub fn run_runtime_tests() {
	test_ownership_drop_queries_do_not_cache_cycle_truncated_children()
	test_ownership_drop_queries_distinguish_type_variants_and_generic_contexts()
	test_ownership_drop_queries_are_invalidated_after_collection()
	test_ownership_drop_queries_are_private_to_the_function_and_worker()
	test_ownership_drop_name_collection_keeps_all_destructors()
	test_ownership_results_survive_batch_arena_release()
	test_storage_query_results_survive_nested_arena_release()
	test_storage_query_views_keep_parent_identity_tables_private()
	test_storage_query_views_preserve_cold_declaration_lookups()
	test_complete_empty_summaries_remove_only_their_own_guard()
	println("runtime metadata assertions passed")
}
')!
	entry := os.join_path(root, 'cmd', 'v', 'v.v')
	os.mkdir_all(os.dir(entry))!
	os.write_file(entry, 'module main

import v.types

fn main() { types.run_runtime_tests() }
')!
	executable := os.join_path(root, 'runtime_metadata${suffix}')
	compiled := os.exec([bootstrap, '-no-retry-compilation', '-cc', 'clang', '-d', 'ownership',
		'-prealloc', '-path', '${overlay_vlib}|${ownership_runtime_vlib_dir}|@vlib|@vmodules',
		'-o', executable, entry])
	assert compiled.exit_code == 0, compiled.output
	result := os.exec([executable])
	assert result.exit_code == 0, result.output
	assert result.output.trim_space() == 'runtime metadata assertions passed', result.output
}
