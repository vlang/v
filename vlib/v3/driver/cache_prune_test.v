module driver

import os
import v3.pref

fn test_whole_program_cache_is_not_persistent_for_test_inputs() {
	assert persistent_program_cache_enabled(true, false, os.join_path(os.temp_dir(), 'v3_cache'))
	assert !persistent_program_cache_enabled(true, true, os.join_path(os.temp_dir(), 'v3_cache'))
	assert !persistent_program_cache_enabled(false, false, os.join_path(os.temp_dir(), 'v3_cache'))
	assert !persistent_program_cache_enabled(true, false, os.join_path(os.temp_dir(),
		'tsession_test'))
}

fn test_builtin_bundle_module_inputs_do_not_reuse_the_bundle_object() {
	assert input_owns_builtin_bundle_module(os.join_path(@VEXEROOT, 'vlib', 'math', 'bits',
		'bits_test.v'), @VEXEROOT)
	assert input_owns_builtin_bundle_module(os.join_path(@VEXEROOT, 'vlib', 'strings'), @VEXEROOT)
	assert !input_owns_builtin_bundle_module(os.join_path(@VEXEROOT, 'vlib', 'math', 'math_test.v'),
		@VEXEROOT)
}

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

fn test_cache_native_public_include_strips_conventional_implementation_macros() {
	include := cache_native_public_include('/tmp/native.h', [
		'#define FEATURE 1',
		'#define FONTSTASH_IMPLEMENTATION',
		'#define SOKOL_FONTSTASH_IMPL',
	], map[string]bool{})
	assert include.contains('#define FEATURE 1')
	assert include.contains('#undef FONTSTASH_IMPLEMENTATION')
	assert include.contains('#undef SOKOL_FONTSTASH_IMPL')
	assert include.contains('#undef SOKOL_IMPL')
	include_pos := include.index('#include') or { -1 }
	assert include_pos >= 0
	// The switches are undefined while the header expands, then restored after it
	// so later native directives in the same unit see the original macro state.
	assert (include.index('#undef FONTSTASH_IMPLEMENTATION') or { -1 }) < include_pos
	assert (include.index('#undef SOKOL_FONTSTASH_IMPL') or { -1 }) < include_pos
	assert (include.last_index('#define FONTSTASH_IMPLEMENTATION') or { -1 }) > include_pos
	assert (include.last_index('#define SOKOL_FONTSTASH_IMPL') or { -1 }) > include_pos
}

fn test_cache_native_public_include_detects_external_function_implementation_macro() {
	dir := os.join_path(os.vtmp_dir(), 'v3_external_implementation_macro_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	header := os.join_path(dir, 'native.h')
	os.write_file(header, 'typedef struct { int value; } V3LibType;
#ifdef LIB_IMPL
int v3_lib_value(void) { return 42; }
#endif
')!
	real_header := os.real_path(header)
	implementation_macros := cache_native_implementation_context_macros(real_header, [
		'#define LIB_IMPL',
	], {
		real_header: true
	}, []string{}, 'cc', pref.host_target())
	assert implementation_macros['LIB_IMPL']
	include := cache_native_public_include(real_header, ['#define LIB_IMPL'], implementation_macros)
	assert include.contains('#undef LIB_IMPL')
	include_pos := include.index('#include') or { -1 }
	assert include_pos >= 0
	assert (include.index('#undef LIB_IMPL') or { -1 }) < include_pos
	// The implementation switch is restored after the header expands.
	assert (include.last_index('#define LIB_IMPL') or { -1 }) > include_pos
	// A gated external definition is stripped, so splitting the root is safe.
	assert !cache_native_public_include_replays_external_definition(real_header, ['#define LIB_IMPL'],
		implementation_macros, []string{}, 'cc', pref.host_target())
}

fn test_cache_native_public_include_replays_unconditional_external_definition() {
	dir := os.join_path(os.vtmp_dir(), 'v3_unconditional_external_definition_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	header := os.join_path(dir, 'native.h')
	os.write_file(header, 'typedef struct { int value; } V3LibType;
int v3_unconditional_helper(void) { return 7; }
')!
	real_header := os.real_path(header)
	// No context define gates the definition, so the declaration-only replay would
	// still emit v3_unconditional_helper and duplicate the owner symbol.
	assert cache_native_public_include_replays_external_definition(real_header, []string{},
		map[string]bool{}, []string{}, 'cc', pref.host_target())
}

fn test_cache_native_public_include_keeps_static_definition_private() {
	dir := os.join_path(os.vtmp_dir(), 'v3_static_definition_private_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	header := os.join_path(dir, 'native.h')
	os.write_file(header, 'typedef struct { int value; } V3LibType;
static int v3_private_helper(void) { return 7; }
')!
	real_header := os.real_path(header)
	// A static definition has internal linkage, so replaying it in several units
	// cannot collide; splitting stays safe.
	assert !cache_native_public_include_replays_external_definition(real_header, []string{},
		map[string]bool{}, []string{}, 'cc', pref.host_target())
}

fn test_cache_native_public_include_sees_through_static_storage_macros() {
	dir := os.join_path(os.vtmp_dir(), 'v3_macro_static_definition_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	header := os.join_path(dir, 'native.h')
	os.write_file(header, '#define V3_LOCAL static inline
typedef struct { int value; } V3MacroStaticType;
V3_LOCAL V3MacroStaticType v3_macro_static_make(void) {
	V3MacroStaticType r = {41};
	return r;
}
')!
	real_header := os.real_path(header)
	// The storage class is hidden behind V3_LOCAL, as builtin closure headers do
	// with V_CLOSURE_STATIC_INLINE. Preprocessing expands it to `static inline`, so
	// the helper recognizes the internal linkage and keeps splitting enabled.
	assert !cache_native_public_include_replays_external_definition(real_header, []string{},
		map[string]bool{}, []string{}, 'cc', pref.host_target())
}

fn test_cache_c_flags_without_forced_inputs_drops_forced_files() {
	filtered := cache_c_flags_without_forced_inputs(['-DFEATURE=1', '-include', '/tmp/forced.h',
		'-I/tmp/inc', '-imacros', '/tmp/macros.h', '-DOTHER'])
	assert filtered == ['-DFEATURE=1', '-I/tmp/inc', '-DOTHER']
}

fn test_c_source_file_scope_identifiers_excludes_function_bodies_and_directives() {
	identifiers := c_source_file_scope_identifiers('#define SYSTEM_HELPER() ignored_helper()
#define LOCAL_FN(name) static int name(void)
LOCAL_FN(macro_helper) {
	return system_helper();
}
static int local_state;
')
	assert identifiers['LOCAL_FN']
	assert identifiers['macro_helper']
	assert identifiers['local_state']
	assert !identifiers['ignored_helper']
	assert !identifiers['system_helper']
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

fn test_cache_compiler_macro_probe_uses_implicit_objective_c_language() {
	$if macos {
		macros, complete := cache_c_compiler_predefined_macros([]string{}, 'cc',
			pref.host_target(), true)
		assert complete
		assert '__OBJC__' in macros
	}
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
	source := '#define FEATURE 1\n#define LEVEL 2\n#if FEATURE == 1\nstatic int matching_api(void) { return 1; }\n#endif\n#if FEATURE != 1\nstatic int mismatched_api(void) { return 2; }\n#else\nstatic int comparison_fallback_api(void) { return 3; }\n#endif\n#if LEVEL >= 2\nstatic int ordered_api(void) { return 4; }\n#endif\n#if FEATURE + 1 == 2\nstatic int arithmetic_api(void) { return 5; }\n#endif\n#if FEATURE + 1\nstatic int direct_arithmetic_api(void) { return 6; }\n#endif\n#if FEATURE - 1\nstatic int zero_arithmetic_api(void) { return 7; }\n#endif\n'
	mut macros := cache_local_c_flag_macros([]string{})
	active := cache_c_source_definitely_active_code(source, mut macros)
	assert active.contains('matching_api')
	assert !active.contains('mismatched_api')
	assert active.contains('comparison_fallback_api')
	assert active.contains('ordered_api')
	assert active.contains('arithmetic_api')
	assert active.contains('direct_arithmetic_api')
	assert !active.contains('zero_arithmetic_api')
}

fn test_cache_c_source_definitely_active_code_rejects_unresolved_definition_guards() {
	source := '#if __has_builtin(__builtin_add_overflow)\nstatic int builtin_api(void) { return 1; }\n#else\nstatic int fallback_api(void) { return 0; }\n#endif\n'
	mut macros := cache_local_c_compiler_macros([]string{}, 'clang', pref.host_target())
	active, complete := cache_c_source_definitely_active_code_with_status(source, mut macros)
	assert !active.contains('builtin_api')
	assert !active.contains('fallback_api')
	assert !complete

	body_guard_source := 'static int builtin_api(void) {\n#if __has_builtin(__builtin_add_overflow)\n\treturn 1;\n#else\n\treturn 0;\n#endif\n}\n'
	mut body_macros := cache_local_c_compiler_macros([]string{}, 'clang', pref.host_target())
	_, body_complete := cache_c_source_definitely_active_code_with_status(body_guard_source, mut
		body_macros)
	assert body_complete
}

fn test_cache_c_source_definitely_active_code_accepts_local_header_guards() {
	source := '#ifndef LOCAL_API_H\n#define LOCAL_API_H\n#ifndef LOCAL_INLINE\n#define LOCAL_INLINE static inline\n#endif\nLOCAL_INLINE int local_api(void) { return 1; }\n#endif\n'
	mut macros := cache_local_c_compiler_macros([]string{}, 'clang', pref.host_target())
	active, complete := cache_c_source_definitely_active_code_with_status(source, mut macros)
	assert complete
	assert active.contains('local_api')
	assert macros['LOCAL_API_H'].is_defined
	assert macros['LOCAL_INLINE'].is_defined
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

	os.write_file(header_path,
		'#if __has_builtin(__builtin_add_overflow)\nstatic int builtin_api(void) { return 2; }\n#endif\n') or {
		panic(err)
	}
	mut uncertain_paths := map[string]bool{}
	mut uncertain_macros := cache_local_c_compiler_macros([]string{}, 'clang', pref.host_target())
	_, declarations_complete := cache_c_source_definitely_active_code_for_path_with_status(root_path,
		allowed_paths, mut uncertain_paths, mut uncertain_macros, false)
	assert !declarations_complete
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

