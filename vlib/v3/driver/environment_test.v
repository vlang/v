module driver

import os
import v3.ansi
import v3.flat

fn restore_driver_environment(name string, old_value string, was_set bool) {
	if was_set {
		os.setenv(name, old_value, true)
	} else {
		os.unsetenv(name)
	}
}

fn test_v3_environment_coverage_dir_reads_vcovdir() {
	name := 'VCOVDIR'
	old_value := os.getenv(name)
	was_set := name in os.environ()
	defer {
		restore_driver_environment(name, old_value, was_set)
	}
	os.unsetenv(name)
	assert v3_environment_coverage_dir() == ''
	path := os.join_path(os.temp_dir(), 'v3_environment_coverage_${os.getpid()}')
	os.setenv(name, path, true)
	assert v3_environment_coverage_dir() == os.real_path(path)
}

fn test_v3_environment_run_only_reads_vtest_only_fn() {
	name := 'VTEST_ONLY_FN'
	old_value := os.getenv(name)
	was_set := name in os.environ()
	defer {
		restore_driver_environment(name, old_value, was_set)
	}
	os.unsetenv(name)
	assert v3_environment_run_only() == []
	os.setenv(name, 'test_one,test_two', true)
	assert v3_environment_run_only() == ['test_one', 'test_two']
}

fn test_v3_environment_show_test_stats_reads_vtest_show_asserts() {
	name := 'VTEST_SHOW_ASSERTS'
	old_value := os.getenv(name)
	was_set := name in os.environ()
	defer {
		restore_driver_environment(name, old_value, was_set)
	}
	os.unsetenv(name)
	assert !v3_environment_show_test_stats()
	os.setenv(name, '1', true)
	assert v3_environment_show_test_stats()
}

fn test_v3_diagnostic_color_option() {
	defer {
		apply_v3_diagnostic_color_option('-color')
	}
	apply_v3_diagnostic_color_option('-nocolor')
	assert ansi.red('error') == 'error'
	apply_v3_diagnostic_color_option('-color')
	assert ansi.red('error') == '\x1b[31merror\x1b[39m'
}

fn test_parallel_cc_external_definition_precheck_uses_active_ast_directives() {
	root := os.join_path(os.temp_dir(), 'v3_parallel_cc_active_directive_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'fn main() {}\n')!
	os.write_file(os.join_path(root, 'windows_impl.h'), 'int windows_impl(void) { return 1; }\n')!
	mut a := &flat.FlatAst{
		nodes: [
			flat.Node{
				kind:  .file
				value: source
			},
			flat.Node{
				kind:  .directive
				value: 'include'
				typ:   '"@DIR/windows_impl.h"'
			},
		]
	}
	assert v3_parallel_cc_active_sources_include_external_definition(a, [source])
	a.nodes[1] = flat.Node{}
	assert !v3_parallel_cc_active_sources_include_external_definition(a, [source])
}

fn test_v3_run_only_cache_identity_distinguishes_patterns() {
	assert v3_run_only_cache_identity([]) == ''
	first := v3_run_only_cache_identity(['test_one'])
	second := v3_run_only_cache_identity(['test_two'])
	assert first != second
	left := v3_run_only_cache_identity(['a', 'bc'])
	right := v3_run_only_cache_identity(['ab', 'c'])
	assert left != right
}

fn test_v3_effective_warns_are_errors_includes_prod() {
	assert !v3_effective_warns_are_errors(false, false)
	assert v3_effective_warns_are_errors(true, false)
	assert v3_effective_warns_are_errors(false, true)
	assert v3_effective_warns_are_errors(true, true)
}
