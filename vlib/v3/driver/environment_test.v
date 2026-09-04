module driver

import os
import crypto.sha256
import v3.ansi
import v3.flat
import v3.parser
import v3.pref
import v3.types

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

fn test_single_moduleless_test_does_not_duplicate_a_resolvable_same_dir_module() {
	root := os.join_path(os.temp_dir(), 'v3_same_dir_test_import_${os.getpid()}')
	os.rmdir_all(root) or {}
	module_dir := os.join_path(root, 'vlib', 'sample')
	os.mkdir_all(module_dir)!
	defer {
		os.rmdir_all(root) or {}
	}
	test_file := os.join_path(module_dir, 'sample_test.v')
	module_file := os.join_path(module_dir, 'sample.v')
	os.write_file(test_file, 'import sample\n\nfn test_sample() {}\n')!
	os.write_file(module_file, 'module sample\n\npub fn value() int { return 1 }\n')!
	mut prefs := pref.new_preferences()
	prefs.vroot = root
	assert same_dir_module_source_files(test_file, '', prefs) == []
}

fn test_single_moduleless_test_keeps_an_unresolvable_same_dir_fixture_module() {
	root := os.join_path(os.temp_dir(), 'v3_same_dir_test_fixture_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	test_file := os.join_path(root, 'fixture_test.v')
	module_file := os.join_path(root, 'helper.v')
	os.write_file(test_file, 'import helper\n\nfn test_helper() {}\n')!
	os.write_file(module_file, 'module helper\n\npub fn value() int { return 1 }\n')!
	mut prefs := pref.new_preferences()
	prefs.vroot = os.join_path(root, 'toolchain')
	assert same_dir_module_source_files(test_file, '', prefs) == [module_file]
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

fn test_release_unused_diagnostic_scope_rebinds_notices() {
	mut notices := []types.TypeError{cap: 1}
	scope := prealloc_scope_begin_for_v3()
	notices << types.TypeError{ msg: 'first' }
	notices << types.TypeError{ msg: 'second' }
	$if prealloc {
		assert scoped_value_owned(scope, notices.data)
	}
	release_unused_diagnostic_scope(mut notices, scope)
	assert notices.len == 0
	assert notices.cap == 0
	notices << types.TypeError{ msg: 'parent owned' }
	assert notices[0].msg == 'parent owned'
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
	backend_builtin_path := os.join_path(root, 'vlib', 'builtin', 'ownership_interface_d_v3_backend.v')
	shared_builtin_path := os.join_path(root, 'vlib', 'builtin', 'internal.v')
	prealloc_builtin_path := os.join_path(root, 'vlib', 'builtin', 'prealloc.c.v')
	shared_vlib_path := os.join_path(root, 'vlib', 'os', 'shared.v')
	os.mkdir_all(os.dir(backend_builtin_path))!
	os.mkdir_all(os.dir(shared_vlib_path))!
	parsed_source := 'module main\nfn main() { println(42) }\n'
	shared_builtin_source := 'module builtin\nfn shared_builtin_input() {}\n'
	shared_vlib_source := 'module os\nfn shared_input() {}\n'
	os.write_file(path, parsed_source)!
	os.write_file(backend_builtin_path, 'module builtin\nfn v3_backend_only() {}\n')!
	os.write_file(prealloc_builtin_path, 'module builtin\nfn prealloc_only() {}\n')!
	os.write_file(shared_builtin_path, shared_builtin_source)!
	os.write_file(shared_vlib_path, shared_vlib_source)!
	prefs := pref.new_preferences()
	mut p := parser.Parser.new(prefs)
	p.parse_into(path)
	p.parse_into(backend_builtin_path)
	p.parse_into(prealloc_builtin_path)
	p.parse_into(shared_builtin_path)
	p.parse_into(shared_vlib_path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	// Replacing the file after parsing must not change the staged digest.
	os.write_file(path, parsed_source.replace('42', 'private_value'))!
	cached_source := os.join_path(root, 'cached', 'module.v')
	warmup_source := os.join_path(root, 'cached', 'warmup.v')
	os.mkdir_all(os.dir(cached_source))!
	cached_source_text := 'module cached\npub fn cached_input() {}\n'
	warmup_source_text := 'module warmup\npub fn unused_warmup() {}\n'
	os.write_file(cached_source, cached_source_text)!
	os.write_file(warmup_source, warmup_source_text)!
	sources := macos_v3_fallback_report_sources(p.a, root, {
		os.real_path(cached_source): sha256.hexhash(cached_source_text)
		os.real_path(warmup_source): sha256.hexhash(warmup_source_text)
	}, {
		os.real_path(warmup_source): true
	})
	real_path := os.real_path(path)
	assert sources[real_path] == sha256.hexhash(parsed_source)
	assert sources[real_path] != sha256.hexhash(os.read_file(path)!)
	// Only the opposite v3_backend compiler-support variants are excluded. Shared
	// builtin and other bundled vlib inputs remain protected by their parser digests.
	assert os.real_path(backend_builtin_path) !in sources
	assert os.real_path(prealloc_builtin_path) !in sources
	assert sources[os.real_path(shared_builtin_path)] == sha256.hexhash(shared_builtin_source)
	assert sources[os.real_path(shared_vlib_path)] == sha256.hexhash(shared_vlib_source)
	assert sources[os.real_path(cached_source)] == sha256.hexhash(cached_source_text)
	assert os.real_path(warmup_source) !in sources
	report_dir := os.join_path(root, 'report')
	assert stage_macos_v3_fallback_source_digests(report_dir, sources)
	staged_paths := os.read_file(os.join_path(report_dir, macos_v3_c_error_v_sources_file))!
	staged_digests :=
		os.read_file(os.join_path(report_dir, macos_v3_c_error_v_source_digests_file))!
	assert staged_paths == [os.real_path(cached_source), real_path,
		os.real_path(shared_builtin_path), os.real_path(shared_vlib_path)].join('\x00')
	assert staged_digests == [sha256.hexhash(cached_source_text), sha256.hexhash(parsed_source),
		sha256.hexhash(shared_builtin_source), sha256.hexhash(shared_vlib_source)].join('\x00')
}

fn test_macos_v3_fallback_report_inputs_snapshot_native_dependencies() {
	root := os.join_path(os.temp_dir(), 'v3_fallback_native_digest_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	header_candidate := os.join_path(root, 'project.h')
	source_candidate := os.join_path(root, 'project.c')
	header_source := '#define PROJECT_VALUE 41\n'
	native_source := '#include "project.h"\nint project_value(void) { return PROJECT_VALUE; }\n'
	os.write_file(header_candidate, header_source)!
	os.write_file(source_candidate, native_source)!
	header_path := os.real_path(header_candidate)
	source_path := os.real_path(source_candidate)
	header_digest := sha256.hexhash(header_source)
	source_digest := sha256.hexhash(native_source)
	state := V3ModuleCacheState{
		module_external_inputs: {
			'main': [header_path, source_path]
		}
		module_native_roots: {
			'main': [source_path]
		}
		external_input_digests: {
			header_path: header_digest
			source_path: source_digest
		}
		external_inputs_ready: true
		external_inputs_complete: true
	}
	// A watcher can replace a root after traversal. The fallback manifest must retain
	// the digest captured from the bytes that selected the original dependency tree.
	os.write_file(header_path, '#include "late.h"\n#define PROJECT_VALUE 42\n')!
	inputs := macos_v3_fallback_report_inputs({
		'/project/main.v': sha256.hexhash('module main')
	}, &state)
	assert inputs['/project/main.v'] == sha256.hexhash('module main')
	assert inputs[v3_fallback_native_manifest_key] == sha256.hexhash(v3_fallback_native_manifest_value)
	assert inputs['${v3_fallback_native_input_prefix}${header_path}'] == header_digest
	assert inputs['${v3_fallback_native_input_prefix}${source_path}'] == source_digest
	assert inputs['${v3_fallback_native_input_prefix}${header_path}'] != sha256.hexhash(os.read_file(header_path)!)
}

fn test_v3_fallback_ignores_only_warmup_only_module_sources() {
	hash_source := os.real_path(os.join_path(os.vtmp_dir(), 'v3_fallback_hash.v'))
	mut state := V3ModuleCacheState{
		module_sources: {
			'hash': [hash_source]
		}
		fallback_required_modules: map[string]bool{}
		fallback_warmup_modules: {
			'hash': true
		}
	}
	assert v3_fallback_ignored_warmup_source_paths(state)[hash_source]
	// A later real/transitive import of a module first seen through the synthetic
	// warm-up makes its source set part of the shared V3/V1 manifest again.
	record_v3_fallback_module_use(mut state, 'hash', false)
	assert hash_source !in v3_fallback_ignored_warmup_source_paths(state)
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
				kind: .file
				value: source
			},
			flat.Node{
				kind: .directive
				value: 'include'
				typ: '"@DIR/windows_impl.h"'
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
	os.write_file(clean_file, "// C.comment() and JS.comment()\nfn clean() { println('C.foo JS.bar') }\n")!
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
	os.write_file(string_file, "module main\nconst import_text = 'import sokol.sapp'\nfn string_only() {}\n")!
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

fn test_v3_test_openssl_probe_matches_windows_ci_suppression() {
	assert !v3_test_openssl_probe_allowed('windows-test', 'windows')
	assert v3_test_openssl_probe_allowed('', 'windows')
	assert v3_test_openssl_probe_allowed('linux-test', 'linux')
}

fn test_v3_test_openssl_probe_uses_version_subcommand() {
	probe := v3_test_openssl_dependency_probe('openssl', 'openssl')
	assert probe.command == 'openssl'
	assert probe.args == ['version']
	assert probe.pkgconfig_name == 'openssl'
}

fn test_v3_test_standard_dependency_probes_match_test_runner() {
	node := v3_test_standard_dependency_probe('present_node') or { panic('missing Node probe') }
	assert node.command == 'node'
	assert node.args == ['--version']
	assert node.pkgconfig_name == ''

	python := v3_test_standard_dependency_probe('present_python') or {
		panic('missing Python probe')
	}
	assert python.command == 'python'
	assert python.args == ['--version']
	assert python.pkgconfig_name == 'python3'

	ruby := v3_test_standard_dependency_probe('present_ruby') or { panic('missing Ruby probe') }
	assert ruby.command == 'ruby'
	assert ruby.args == ['--version']
	assert ruby.pkgconfig_name == 'ruby'

	go_probe := v3_test_standard_dependency_probe('present_go') or { panic('missing Go probe') }
	assert go_probe.command == 'go'
	assert go_probe.args == ['version']
	assert go_probe.pkgconfig_name == ''
}

fn test_v3_test_build_defines_populates_referenced_standard_dependencies() {
	name := 'VBUILD_DEFINES'
	old_value := os.getenv(name)
	was_set := name in os.environ()
	defer {
		restore_driver_environment(name, old_value, was_set)
	}
	os.unsetenv(name)
	for define in ['present_node', 'present_python', 'present_ruby', 'present_go'] {
		probe := v3_test_standard_dependency_probe(define) or {
			assert false, 'missing dependency probe for ${define}'
			continue
		}
		defines := v3_test_build_defines('${define}?', [])
		assert (define in defines) == v3_test_dependency_probe_present(probe)
	}
}

fn test_v3_build_constraints_are_evaluated_only_for_direct_tests() {
	root := os.join_path(os.temp_dir(), 'v3_test_constraint_routing_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	malformed_file := os.join_path(root, 'ordinary.v')
	false_test_file := os.join_path(root, 'false_test.v')
	os.write_file(malformed_file, '// vtest build: ((malformed\nmodule main\n')!
	os.write_file(false_test_file, '// vtest build: windows && linux\nmodule main\n')!
	target := pref.host_target()
	assert !v3_direct_test_input_is_incompatible(false, malformed_file, 'c', target, 'clang', false, [])
	assert v3_direct_test_input_is_incompatible(true, false_test_file, 'c', target, 'clang', false, [])
}

fn test_v3_test_sqlite_present_uses_bundled_windows_source() {
	root := os.join_path(os.temp_dir(), 'v3_test_sqlite_present_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'thirdparty', 'sqlite'))!
	defer {
		os.rmdir_all(root) or {}
	}
	assert !v3_test_sqlite_present('windows', root)
	os.write_file(os.join_path(root, 'thirdparty', 'sqlite', 'sqlite3.c'), '')!
	assert v3_test_sqlite_present('windows', root)
}

fn test_v3_prod_c_optimization_flags_skip_lto_for_tcc() {
	assert v3_prod_c_optimization_flags(true, false, false, false, false, false, false) == [
		'-O3',
		'-flto',
	]
	assert v3_prod_c_optimization_flags(true, false, false, false, true, false, false) == [
		'-O2',
		'-flto',
	]
	assert v3_prod_c_optimization_flags(true, false, false, false, true, true, false) == [
		'-O2',
		'-flto',
		'-mllvm',
		'-inline-threshold=75',
	]
	assert v3_prod_c_optimization_flags(true, false, false, false, false, false, true) == [
		'-O3',
	]
	assert v3_prod_c_optimization_flags(true, false, true, false, false, false, false) == [
		'-O3',
	]
	assert v3_prod_c_optimization_flags(true, false, false, true, false, false, false) == [
		'-O3',
	]
	assert v3_prod_c_optimization_flags(false, false, false, false, false, false, false) == []
	assert v3_prod_c_optimization_flags(true, true, false, false, false, false, false) == []
	assert !v3_is_large_prod_c_unit(v3_large_prod_c_unit_threshold - 1)
	assert v3_is_large_prod_c_unit(v3_large_prod_c_unit_threshold)
}

fn test_v3_prod_c_object_optimization_flags_keep_cached_objects_out_of_lto() {
	assert v3_prod_c_object_optimization_flags(true, false, false, false, false) == [
		'-O3',
	]
	assert v3_prod_c_object_optimization_flags(true, false, false, true, false) == [
		'-O3',
	]
	assert v3_prod_c_object_optimization_flags(false, false, false, false, false) == []
	assert v3_prod_c_object_optimization_flags(true, true, false, false, false) == []
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
