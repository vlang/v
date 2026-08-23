module driver

import os
import crypto.sha256
import v3.ansi
import v3.flat
import v3.parser
import v3.pref

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

fn test_v3_default_diagnostic_color_uses_environment() {
	name := 'VCOLORS'
	old_value := os.getenv(name)
	was_set := name in os.environ()
	defer {
		restore_driver_environment(name, old_value, was_set)
		apply_v3_diagnostic_color_option('-color')
	}
	os.setenv(name, 'never', true)
	apply_v3_default_diagnostic_color()
	assert ansi.red('error') == 'error'
	os.setenv(name, 'always', true)
	apply_v3_default_diagnostic_color()
	assert ansi.red('error') == '\x1b[31merror\x1b[39m'
}

fn test_macos_v3_fallback_payload_validation() {
	assert macos_v3_fallback_payload_is_valid('compiler_error\nsemantic checking')
	assert macos_v3_fallback_payload_is_valid('compiler_error')
	assert macos_v3_fallback_payload_is_valid('inline_asm')
	assert macos_v3_fallback_payload_is_valid('c_compilation_error')
	assert !macos_v3_fallback_payload_is_valid('')
	assert !macos_v3_fallback_payload_is_valid('compiler')
	assert !macos_v3_fallback_payload_is_valid('compiler_error_partial')
}

fn test_macos_v3_fallback_report_sources_keep_parser_digests() {
	root := os.join_path(os.temp_dir(), 'v3_fallback_source_digest_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	path := os.join_path(root, 'main.v')
	vlib_path := os.join_path(root, 'vlib', 'builtin', 'internal.v')
	os.mkdir_all(os.dir(vlib_path))!
	parsed_source := 'module main\nfn main() { println(42) }\n'
	os.write_file(path, parsed_source)!
	os.write_file(vlib_path, 'module builtin\nfn internal_only() {}\n')!
	prefs := pref.new_preferences()
	mut p := parser.Parser.new(prefs)
	p.parse_into(path)
	p.parse_into(vlib_path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	// Replacing the file after parsing must not change the staged digest.
	os.write_file(path, parsed_source.replace('42', 'private_value'))!
	sources := macos_v3_fallback_report_sources(p.a, root)
	real_path := os.real_path(path)
	assert sources[real_path] == sha256.hexhash(parsed_source)
	assert sources[real_path] != sha256.hexhash(os.read_file(path)!)
	// Bundled compiler-support sources deliberately differ between V1 and V3 and are
	// not caller inputs, so they do not make every exact-source verification fail.
	assert os.real_path(vlib_path) !in sources
	report_dir := os.join_path(root, 'report')
	assert stage_macos_v3_fallback_source_digests(report_dir, sources)
	assert os.read_file(os.join_path(report_dir, macos_v3_c_error_v_sources_file))! == real_path
	assert os.read_file(os.join_path(report_dir, macos_v3_c_error_v_source_digests_file))! == sha256.hexhash(parsed_source)
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

fn test_impure_v_diagnostics_inspect_ast_nodes_in_every_pure_v_file() {
	root := os.join_path(os.temp_dir(), 'v3_impure_v_ast_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	clean_file := os.join_path(root, 'clean.v')
	c_file := os.join_path(root, 'c_usage.v')
	js_file := os.join_path(root, 'js_usage.v')
	allowed_c_file := os.join_path(root, 'allowed.c.v')
	allowed_js_file := os.join_path(root, 'allowed.js.v')
	os.write_file(clean_file,
		"// C.comment() and JS.comment()\nfn clean() { println('C.foo JS.bar') }\n")!
	os.write_file(c_file, 'fn C.do_work()\nfn use_c(value &C.Widget) { C.do_work() }\n')!
	os.write_file(js_file, 'fn JS.do_work()\nfn use_js(value JS.Number) { JS.do_work() }\n')!
	os.write_file(allowed_c_file, 'fn C.allowed()\nfn use_c() { C.allowed() }\n')!
	os.write_file(allowed_js_file, 'fn JS.allowed()\nfn use_js() { JS.allowed() }\n')!
	prefs := pref.new_preferences()
	mut p := parser.Parser.new(prefs)
	a := p.parse_files([clean_file, c_file, js_file, allowed_c_file, allowed_js_file])
	diagnostics := v3_impure_v_diagnostics(a)
	assert !diagnostics.any(it.file == clean_file), diagnostics.str()
	assert diagnostics.any(it.file == c_file && it.message.starts_with('C code will not be allowed')), diagnostics.str()

	assert diagnostics.any(it.file == js_file
		&& it.message.starts_with('JS code will not be allowed')), diagnostics.str()

	assert !diagnostics.any(it.file == allowed_c_file), diagnostics.str()
	assert !diagnostics.any(it.file == allowed_js_file), diagnostics.str()
}

fn test_wayland_gg_precheck_inspects_parsed_imports_in_every_user_file() {
	root := os.join_path(os.temp_dir(), 'v3_wayland_imports_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	comment_file := os.join_path(root, 'comment.v')
	string_file := os.join_path(root, 'string.v')
	gg_file := os.join_path(root, 'gg.v')
	sapp_file := os.join_path(root, 'sapp.v')
	os.write_file(comment_file, 'module main\n// import gg\nfn comment_only() {}\n')!
	os.write_file(string_file,
		"module main\nconst import_text = 'import sokol.sapp'\nfn string_only() {}\n")!
	os.write_file(gg_file, 'module main\nimport gg\nfn gg_import() {}\n')!
	os.write_file(sapp_file, 'module main\nimport sokol.sapp\nfn sapp_import() {}\n')!
	prefs := pref.new_preferences()
	mut p := parser.Parser.new(prefs)
	a := p.parse_files([comment_file, string_file, gg_file, sapp_file])
	assert !parsed_files_import_linux_gg(a, [comment_file, string_file])
	directory_files := v3_directory_user_files(root, prefs, false, false)!
	assert directory_files.len == 4
	assert parsed_files_import_linux_gg(a, directory_files)
	assert parsed_files_import_linux_gg(a, [sapp_file])
}

fn test_linux_wayland_only_session_matches_established_compiler_detection() {
	assert is_linux_wayland_only_session('linux', '', 'wayland-0', '')
	assert is_linux_wayland_only_session('linux', '', '', 'Wayland')
	assert !is_linux_wayland_only_session('linux', ':0', 'wayland-0', 'wayland')
	assert !is_linux_wayland_only_session('macos', '', 'wayland-0', 'wayland')
	assert !is_linux_wayland_only_session('linux', '', '', 'x11')
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

fn test_v3_prod_c_optimization_flags_skip_lto_for_tcc() {
	assert v3_prod_c_optimization_flags(true, false, false, false, false) == ['-O3', '-flto']
	assert v3_prod_c_optimization_flags(true, false, false, false, true) == ['-O3']
	assert v3_prod_c_optimization_flags(true, false, true, false, false) == ['-O3']
	assert v3_prod_c_optimization_flags(true, false, false, true, false) == ['-O3']
	assert v3_prod_c_optimization_flags(false, false, false, false, false) == []
	assert v3_prod_c_optimization_flags(true, true, false, false, false) == []
}

fn test_effective_c_compiler_name_detects_path_valued_tcc() {
	target := pref.target_from('macos', 'amd64')!
	assert effective_c_compiler_name('/opt/tcc/bin/tcc', target) == 'tinyc'
}

fn test_v3_windows_batch_command_uses_windows_quoting() {
	command := v3_windows_batch_command('C:\\Program Files\\LLVM\\clang.exe', [
		'-IC:\\SDK Files\\include',
		'-DNAME="V compiler"',
		'100% ready!',
	])
	assert command.starts_with('"C:\\Program Files\\LLVM\\clang.exe" ')
	assert command.contains('"-IC:\\SDK Files\\include"')
	assert command.contains('"-DNAME=\\"V compiler\\""')
	assert command.ends_with('"100%% ready!"')
	assert !command.contains("'C:\\Program Files")
}

fn test_v3_posix_shell_command_quotes_every_argument() {
	command := v3_posix_shell_command('clang', [r'/tmp/proj\name', 'plain', "it's"])
	assert command == "'clang' '/tmp/proj\\name' 'plain' 'it'\\''s'"
}

fn test_record_user_define_normalizes_nonempty_valued_defines() {
	mut defines := []string{}
	mut values := map[string]string{}
	record_user_define(mut defines, mut values, 'feature=enabled')
	assert defines == ['feature', 'feature=enabled']
	assert values['feature'] == 'enabled'

	record_user_define(mut defines, mut values, 'empty=')
	assert 'empty' !in defines
	assert 'empty=' in defines
	assert values['empty'] == ''
}
