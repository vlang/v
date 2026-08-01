module driver

import os
import v3.pref

fn test_cache_function_reference_counts_scans_source_once() {
	candidates := {
		'alpha__one': true
		'beta__two':  true
	}
	counts := cache_function_reference_counts('void alpha__one(void); alpha__one(); beta__two(); beta__two_extra(); alpha__one();',
		candidates)
	assert counts['alpha__one'] == 2
	assert counts['beta__two'] == 1
}

fn test_c_source_references_identifiers_ignores_comments_strings_and_longer_names() {
	identifiers := {
		'local_helper': true
	}
	assert c_source_references_identifiers('int call(void) { return local_helper(); }', identifiers)
	assert c_source_references_identifiers('#define CALL_LOCAL() local_helper()\n', identifiers)
	assert !c_source_references_identifiers('// local_helper()\n/* local_helper */\nconst char *name = "local_helper";\nint local_helper_extra(void);\n',
		identifiers)
}

fn test_v_c_identifiers_accepts_spaced_and_commented_selectors() {
	assert v_c_identifiers('C /* selector */ . helper()\nC\n.\nother()\nC // line comment\n. line_helper()') == [
		'helper',
		'other',
		'line_helper',
	]
	assert v_c_identifiers('// C.fake()\nC.real_value') == [
		'real_value',
	]
}

fn test_cache_c_source_definitely_active_code_filters_conditional_definitions() {
	source := 'int always_active(void) { return 1; }\n#if FEATURE\nstatic int bundled_api(void) { return 2; }\n#else\nint library_api(void) { return 3; }\n#endif\n'
	mut disabled_macros := cache_local_c_flag_macros(['-DFEATURE=0'])
	disabled := cache_c_source_definitely_active_code(source, mut disabled_macros)
	assert disabled.contains('always_active')
	assert !disabled.contains('bundled_api')
	assert disabled.contains('library_api')

	mut enabled_macros := cache_local_c_flag_macros(['-DFEATURE=1'])
	enabled := cache_c_source_definitely_active_code(source, mut enabled_macros)
	assert enabled.contains('always_active')
	assert enabled.contains('bundled_api')
	assert !enabled.contains('library_api')

	mut unknown_macros := cache_local_c_flag_macros([])
	unknown := cache_c_source_definitely_active_code(source, mut unknown_macros)
	assert unknown.contains('always_active')
	assert !unknown.contains('bundled_api')
	assert !unknown.contains('library_api')
}

fn test_cache_c_source_definitely_active_code_uses_compiler_predefined_macros() {
	source := '#ifdef __clang__\nstatic int compiler_api(void) { return 1; }\n#else\nint fallback_api(void) { return 2; }\n#endif\n'
	mut clang_macros := cache_local_c_compiler_macros([]string{}, 'clang', pref.host_target())
	clang_source := cache_c_source_definitely_active_code(source, mut clang_macros)
	assert clang_source.contains('compiler_api')
	assert !clang_source.contains('fallback_api')

	mut undefined_macros := cache_local_c_compiler_macros(['-U__clang__'], 'clang',
		pref.host_target())
	undefined_source := cache_c_source_definitely_active_code(source, mut undefined_macros)
	assert !undefined_source.contains('compiler_api')
	assert undefined_source.contains('fallback_api')
}

fn test_cache_c_source_definitely_active_code_uses_target_predefined_macros() {
	target := pref.target_from('macos', 'arm64') or { panic(err) }
	mut macros := cache_local_c_compiler_macros([]string{}, 'clang', target)
	source := '#ifdef __APPLE__\nstatic int apple_api(void) { return 1; }\n#endif\n#if defined(__MACH__) && defined(__aarch64__) && defined(__arm64__) && defined(__LP64__)\nstatic int target_api(void) { return 2; }\n#endif\n'
	active := cache_c_source_definitely_active_code(source, mut macros)
	assert active.contains('apple_api')
	assert active.contains('target_api')

	mut overridden := cache_local_c_compiler_macros(['-U__APPLE__'], 'clang', target)
	disabled := cache_c_source_definitely_active_code(source, mut overridden)
	assert !disabled.contains('apple_api')
}

fn test_cache_c_source_definitely_active_code_evaluates_compound_known_guards() {
	source := '#if FOO && BAR\nstatic int compound_api(void) { return 1; }\n#else\nint fallback_api(void) { return 2; }\n#endif\n'
	mut enabled_macros := cache_local_c_flag_macros(['-DFOO=1', '-DBAR=1'])
	enabled := cache_c_source_definitely_active_code(source, mut enabled_macros)
	assert enabled.contains('compound_api')
	assert !enabled.contains('fallback_api')

	mut short_circuit_macros := cache_local_c_flag_macros(['-DFOO=0'])
	disabled := cache_c_source_definitely_active_code(source, mut short_circuit_macros)
	assert !disabled.contains('compound_api')
	assert disabled.contains('fallback_api')
}

fn test_cache_c_source_definitely_active_code_expands_local_macro_values() {
	source := '#define ENABLED 1\n#define PARENTHESIZED (1)\n#define ALIASED ENABLED\n#define COMPOUND (PARENTHESIZED && ALIASED)\n#if PARENTHESIZED\nstatic int parenthesized_api(void) { return 1; }\n#endif\n#if ALIASED\nstatic int aliased_api(void) { return 2; }\n#endif\n#if COMPOUND\nstatic int compound_macro_api(void) { return 3; }\n#endif\n#define FIRST SECOND\n#define SECOND FIRST\n#if FIRST\nstatic int cyclic_api(void) { return 4; }\n#endif\n'
	mut macros := cache_local_c_flag_macros([]string{})
	active := cache_c_source_definitely_active_code(source, mut macros)
	assert active.contains('parenthesized_api')
	assert active.contains('aliased_api')
	assert active.contains('compound_macro_api')
	assert !active.contains('cyclic_api')
}

fn test_cache_c_source_definitely_active_code_evaluates_known_comparisons() {
	source := '#define FEATURE 1\n#define LEVEL 2\n#if FEATURE == 1\nstatic int matching_api(void) { return 1; }\n#endif\n#if FEATURE != 1\nstatic int mismatched_api(void) { return 2; }\n#else\nstatic int comparison_fallback_api(void) { return 3; }\n#endif\n#if LEVEL >= 2\nstatic int ordered_api(void) { return 4; }\n#endif\n'
	mut macros := cache_local_c_flag_macros([]string{})
	active := cache_c_source_definitely_active_code(source, mut macros)
	assert active.contains('matching_api')
	assert !active.contains('mismatched_api')
	assert active.contains('comparison_fallback_api')
	assert active.contains('ordered_api')
}

fn test_cache_c_source_definitely_active_code_uses_include_site_macros() {
	root_dir := os.join_path(os.temp_dir(), 'v3_cache_active_c_include_${os.getpid()}')
	os.rmdir_all(root_dir) or {}
	os.mkdir_all(root_dir) or { panic(err) }
	defer {
		os.rmdir_all(root_dir) or {}
	}
	root_path := os.join_path(root_dir, 'a_root.c')
	header_path := os.join_path(root_dir, 'z_api.h')
	os.write_file(root_path, '#include "z_api.h"\n#undef FEATURE\n#define FEATURE 1\n') or {
		panic(err)
	}
	os.write_file(header_path, '#if FEATURE\nstatic int bundled_api(void) { return 2; }\n#endif\n') or {
		panic(err)
	}
	allowed_paths := {
		os.real_path(root_path):   true
		os.real_path(header_path): true
	}
	mut active_paths := map[string]bool{}
	mut macros := cache_local_c_flag_macros(['-DFEATURE=0'])
	active := cache_c_source_definitely_active_code_for_path(root_path, allowed_paths, mut
		active_paths, mut macros, false)
	assert !active.contains('bundled_api')
	assert macros['FEATURE'].truth == 1
}

fn test_prune_cached_native_function_prototypes_resolves_cache_guards() {
	state := &V3ModuleCacheState{
		native_declared_functions: {
			'owner': {
				'active_api': true
			}
		}
	}
	source := '#ifndef V3CACHE_PROGRAM_UNIT\nint active_api(void);\n#endif\n#ifndef V3CACHE_PROGRAM_UNIT\nint library_api(void);\n#endif\nint active_api(void);\n'
	pruned := prune_cached_native_function_prototypes(source, state, ['owner'])
	assert !pruned.contains('active_api')
	assert pruned.contains('int library_api(void);')
	assert !pruned.contains('V3CACHE_PROGRAM_UNIT')
}
