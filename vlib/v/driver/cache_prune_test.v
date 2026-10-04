module driver

import os
import v.pref

fn test_module_cache_compiler_identity_changes_when_executable_changes() {
	root := os.join_path(os.vtmp_dir(), 'v3_cache_vexe_identity_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	vexe := os.join_path(root, 'v')
	os.write_file(vexe, 'old compiler')!
	old_identity := v3_cache_compiler_executable_identity(vexe)
	os.write_file(vexe, 'new compiler executable')!
	new_identity := v3_cache_compiler_executable_identity(vexe)
	assert old_identity != new_identity
}

// Without usable file metadata the compiler identity must fall back to the
// executable contents.
fn test_module_cache_compiler_identity_changes_without_file_metadata() {
	root := os.join_path(os.vtmp_dir(), 'v3_cache_vexe_no_metadata_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	name := 'V3_TEST_NO_FILE_METADATA'
	was_set := name in os.environ()
	old_value := os.getenv(name)
	defer {
		if was_set {
			os.setenv(name, old_value, true)
		} else {
			os.unsetenv(name)
		}
		os.rmdir_all(root) or {}
	}
	vexe := os.join_path(root, 'v')
	os.write_file(vexe, 'old compiler')!
	os.setenv(name, vexe, true)
	old_identity := v3_cache_compiler_executable_identity(vexe)
	// Same size, different bytes.
	os.write_file(vexe, 'new compiler')!
	new_identity := v3_cache_compiler_executable_identity(vexe)
	assert old_identity != new_identity
}

// A development compiler reads its embedded C headers at run time, so editing one
// must change the cache identity without a rebuild.
fn test_module_cache_compiler_runtime_inputs_identity_changes_when_a_header_changes() {
	root := os.join_path(os.vtmp_dir(), 'v3_cache_runtime_inputs_${os.getpid()}')
	os.rmdir_all(root) or {}
	dir := os.join_path(root, 'vlib', 'v', 'gen', 'c')
	os.mkdir_all(dir)!
	defer {
		os.rmdir_all(root) or {}
	}
	for name in v3_cache_compiler_runtime_inputs {
		path := os.join_path(dir, name)
		os.write_file(path, 'old header')!
		// Keep a coarse file system clock from giving the rewrite the same time stamp.
		os.utime(path, 1_000_000_000, 1_000_000_000)!
	}
	for name in v3_cache_compiler_runtime_inputs {
		old_identity := v3_cache_compiler_runtime_inputs_identity(root)
		// Same size, different bytes.
		os.write_file(os.join_path(dir, name), 'new header')!
		new_identity := v3_cache_compiler_runtime_inputs_identity(root)
		assert old_identity != new_identity, name
	}
}

// Every `$embed_file` of the compiler is read at run time by a development build,
// so it must be part of the cache identity.
fn test_module_cache_compiler_runtime_inputs_cover_compiler_embedded_files() {
	vlib_v := os.join_path(@VMODROOT, 'vlib', 'v')
	gen_c := os.real_path(os.join_path(vlib_v, 'gen', 'c'))
	mut embedded := map[string]bool{}
	for file in os.walk_ext(vlib_v, '.v') {
		normalized := file.replace('\\', '/')
		if file.ends_with('_test.v') || normalized.contains('/tests/')
			|| normalized.contains('/testdata/') {
			continue
		}
		for line in os.read_lines(file)! {
			code := line.all_before('//')
			if !code.contains('\$embed_file(') {
				continue
			}
			argument := code.all_after('\$embed_file(').trim_space()
			assert argument.len > 1, '${file}: ${line}'
			name := argument[1..].all_before(argument[0..1])
			target := os.real_path(os.join_path(os.dir(file), name))
			assert os.dir(target) == gen_c, '${file}: ${line}'
			assert os.file_name(target) in v3_cache_compiler_runtime_inputs, '${file}: ${line}'
			embedded[os.file_name(target)] = true
		}
	}
	assert embedded.len == v3_cache_compiler_runtime_inputs.len
}

fn test_large_cold_cache_restarts_without_cache() {
	limit := scoped_large_cold_cache_node_limit
	assert should_restart_v3_large_cold_cache(true, true, limit, false, false, false)
	assert !should_restart_v3_large_cold_cache(true, true, limit - 1, false, false, false)
	assert !should_restart_v3_large_cold_cache(false, true, limit, false, false, false)
	assert !should_restart_v3_large_cold_cache(true, false, limit, false, false, false)
	assert !should_restart_v3_large_cold_cache(true, true, limit, true, false, false)
	assert !should_restart_v3_large_cold_cache(true, true, limit, false, true, false)
	assert !should_restart_v3_large_cold_cache(true, true, limit, false, false, true)
}

fn test_large_cold_cache_marker_bypasses_unchanged_sources_and_invalidates_changes() {
	root := os.join_path(os.vtmp_dir(), 'v3_large_cold_cache_marker_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	source_path := os.join_path(root, 'main.v')
	os.write_file(source_path, 'fn main() {}\n')!
	marker_path := v3_large_cold_cache_marker_path(os.join_path(root, 'cache'), source_path, [])
	assert write_v3_large_cold_cache_marker(marker_path, [source_path])
	assert valid_v3_large_cold_cache_marker(marker_path)

	os.write_file(os.join_path(root, 'added.v'), 'fn added() {}\n')!
	assert !valid_v3_large_cold_cache_marker(marker_path)
	assert !os.exists(marker_path)

	assert write_v3_large_cold_cache_marker(marker_path, [source_path])
	assert valid_v3_large_cold_cache_marker(marker_path)
	os.write_file(source_path, 'fn main() { println(1) }\n')!
	assert !valid_v3_large_cold_cache_marker(marker_path)
}

fn test_large_cold_cache_marker_is_scoped_to_the_input_set() {
	cache_dir := os.join_path(os.vtmp_dir(), 'v3_large_cold_cache_marker_paths')
	first := v3_large_cold_cache_marker_path(cache_dir, '/project/main.v', [])
	second := v3_large_cold_cache_marker_path(cache_dir, '/project/other.v', [])
	with_list := v3_large_cold_cache_marker_path(cache_dir, '/project/main.v', [
		'/project/extra.v',
	])
	assert first != second
	assert first != with_list
}

fn test_whole_program_cache_is_not_persistent_for_test_inputs() {
	old_v3cache := os.getenv('V3CACHE')
	had_v3cache := 'V3CACHE' in os.environ()
	defer {
		if had_v3cache {
			os.setenv('V3CACHE', old_v3cache, true)
		} else {
			os.unsetenv('V3CACHE')
		}
	}
	os.unsetenv('V3CACHE')
	assert persistent_program_cache_enabled(true, false, os.join_path(os.temp_dir(), 'v3_cache'))
	assert !persistent_program_cache_enabled(true, true, os.join_path(os.temp_dir(), 'v3_cache'))
	assert !persistent_program_cache_enabled(false, false, os.join_path(os.temp_dir(), 'v3_cache'))
	assert !persistent_program_cache_enabled(true, false, os.join_path(os.temp_dir(),
		'tsession_test'))
	os.setenv('V3CACHE', os.join_path(os.temp_dir(), 'bounded_v3_cache'), true)
	assert persistent_program_cache_enabled(true, false, os.join_path(os.temp_dir(),
		'tsession_test'))
}

fn test_builtin_bundle_module_inputs_do_not_reuse_the_bundle_object() {
	assert input_owns_builtin_bundle_module(os.join_path(@VEXEROOT, 'vlib', 'math', 'bits',
		'bits_test.v'), @VEXEROOT)
	assert input_owns_builtin_bundle_module(os.join_path(@VEXEROOT, 'vlib', 'strings'), @VEXEROOT)
	assert !input_owns_builtin_bundle_module(os.join_path(@VEXEROOT, 'vlib', 'math', 'math_test.v'),
		@VEXEROOT)
}

fn test_cached_object_wrapper_signature_ignores_non_wrapper_prefix_changes() {
	base := 'base signature'
	wrapper := '/* V3CACHE_PROGRAM_WRAPPERS */\nstatic void callback(void) {}\n/* V3CACHE_PROGRAM_WRAPPERS_END */'
	raw_source := '#define NATIVE_IMPLEMENTATION\n#include "native.h"\n${wrapper}\n/* V3CACHE_BODY_BEGIN */\n'
	prepared_source := '#define V3CACHE_PROGRAM_UNIT 1\n${wrapper}\n/* V3CACHE_BODY_BEGIN */\n'
	assert v3_cached_object_wrapper_compile_signature(base, raw_source) == v3_cached_object_wrapper_compile_signature(base,
		prepared_source)
	changed_source := prepared_source.replace('callback(void)', 'other_callback(void)')
	assert v3_cached_object_wrapper_compile_signature(base, prepared_source) != v3_cached_object_wrapper_compile_signature(base,
		changed_source)
	assert v3_cached_object_wrapper_compile_signature(base, 'int declaration;') == base
}

fn test_cached_object_signature_keeps_panic_frame_objects_apart() {
	base := 'base signature'
	plain := 'int declaration;\n/* V3CACHE_BODY_BEGIN */\n'
	with_frames := 'typedef struct v_unwind_frame {\n\tstruct v_unwind_frame* prev;\n} v_unwind_frame;\n/* V3CACHE_BODY_BEGIN */\n'
	assert v3_cached_object_wrapper_compile_signature(base, plain) == base
	assert v3_cached_object_wrapper_compile_signature(base, with_frames) != base
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

fn test_target_libc_cached_prefix_refreshes_when_thread_support_changes() {
	no_threads := '#include <stdint.h>\n'
	type_only := '#include <pthread.h>\ntypedef struct { pthread_t handle; } __v_thread;\n'
	runtime := '${type_only}static __v_thread __v_thread_spawn(void);\n'
	pthread_header := '#include <pthread.h>\n'
	assert target_libc_cached_prefix_needs_thread_refresh(no_threads,
		'void main__main(void) { pthread_self(); }')
	assert !target_libc_cached_prefix_needs_thread_refresh(pthread_header,
		'void main__main(void) { pthread_self(); }')
	assert target_libc_cached_prefix_needs_thread_refresh(no_threads,
		'void main__main(void) { sizeof(__v_thread); }')
	assert !target_libc_cached_prefix_needs_thread_refresh(type_only,
		'void main__main(void) { sizeof(__v_thread); }')
	assert target_libc_cached_prefix_needs_thread_refresh(type_only,
		'void main__main(void) { __v_thread_spawn(); }')
	assert !target_libc_cached_prefix_needs_thread_refresh(runtime,
		'void main__main(void) { __v_thread_spawn(); }')
	assert target_libc_cached_prefix_needs_thread_refresh(runtime,
		'void main__main(void) { sizeof(__v_thread); }')
	assert target_libc_cached_prefix_needs_thread_refresh(runtime, 'void main__main(void) {}')
	assert target_libc_cached_prefix_needs_thread_refresh(type_only, 'void main__main(void) {}')
	assert !target_libc_cached_prefix_needs_thread_refresh(no_threads,
		'// __v_thread pthread_self\nconst char *name = "__v_thread_spawn pthread_create";\n')
}

fn test_cache_native_public_include_strips_conventional_implementation_macros() {
	include := cache_native_public_include('/tmp/native.h', [
		'#define FEATURE 1',
		'#include "/tmp/context.h"',
		'#define FONTSTASH_IMPLEMENTATION',
		'#define SOKOL_FONTSTASH_IMPL',
	], map[string]bool{})
	assert include.contains('#define FEATURE 1')
	assert !include.contains('context.h')
	assert include.contains('#undef FONTSTASH_IMPLEMENTATION')
	assert include.contains('#undef SOKOL_FONTSTASH_IMPL')
	assert include.contains('#undef SOKOL_IMPL')
	include_pos := include.index('#include') or { -1 }
	assert include_pos >= 0
	// Declaration-only replay must leave the switches undefined so later generated
	// wrappers are emitted only by the selected owner object.
	assert (include.index('#undef FONTSTASH_IMPLEMENTATION') or { -1 }) < include_pos
	assert (include.index('#undef SOKOL_FONTSTASH_IMPL') or { -1 }) < include_pos
	assert !include.contains('#define FONTSTASH_IMPLEMENTATION')
	assert !include.contains('#define SOKOL_FONTSTASH_IMPL')
}

fn test_cached_native_owner_restores_stripped_implementation_context() {
	path := os.real_path('/tmp/sokol_gl.h')
	include_line := '#include "${c_include_path(path)}"'
	state := &V3ModuleCacheState{
		module_sources:        {
			'sgl': ['/tmp/sgl.v']
		}
		module_native_roots:   {
			'sgl': [path]
		}
		native_root_contexts:  {
			path: ['#define SOKOL_IMPL']
		}
		native_root_owners:    {
			path: 'sgl'
		}
		native_source_modules: {
			'sgl': true
		}
	}
	native := cache_source_with_cached_native_inputs('/* v3 cache omitted SOKOL_IMPL */\n${include_line}\n',
		state, ['sgl'])
	define_pos := native.source.index('#define SOKOL_IMPL') or { -1 }
	include_pos := native.source.index(include_line) or { -1 }
	assert native.has_native
	assert define_pos >= 0
	assert include_pos > define_pos
}

fn test_cache_c_flags_without_forced_inputs_drops_forced_files() {
	filtered := cache_c_flags_without_forced_inputs(['-DFEATURE=1', '-include', '/tmp/forced.h',
		'-I/tmp/inc', '-imacros', '/tmp/macros.h', '-include=/tmp/joined.h',
		'-imacros=/tmp/joined_macros.h', '-DOTHER'])
	assert filtered == ['-DFEATURE=1', '-I/tmp/inc', '-DOTHER']
}

fn test_cache_probe_language_unions_input_and_flag_languages() {
	assert cache_probe_language('c', []string{}) == 'c'
	assert cache_probe_language('objective-c', []string{}) == 'objective-c'
	assert cache_probe_language('c++', []string{}) == 'c++'
	assert cache_probe_language('objective-c++', []string{}) == 'objective-c++'
	// A C++ input plus a command-line Objective-C request needs both macros.
	assert cache_probe_language('c++', ['-fobjc-arc']) == 'objective-c++'
	assert cache_probe_language('c', ['-x', 'objective-c']) == 'objective-c'
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

fn test_cache_compiler_macro_probe_uses_implicit_objective_c_language() {
	$if macos {
		macros, complete := cache_c_compiler_predefined_macros([]string{}, 'cc',
			pref.host_target(), 'objective-c')
		assert complete
		assert '__OBJC__' in macros
		// An Objective-C++ input defines both __OBJC__ and __cplusplus; the probe must
		// carry __cplusplus so branches guarded by it are not discarded.
		objc_cpp_macros, objc_cpp_complete := cache_c_compiler_predefined_macros([]string{}, 'cc',
			pref.host_target(), 'objective-c++')
		assert objc_cpp_complete
		assert '__OBJC__' in objc_cpp_macros
		assert '__cplusplus' in objc_cpp_macros
	}
}

fn test_cache_compiler_macro_probe_excludes_forced_headers() {
	$if macos {
		dir := os.join_path(os.vtmp_dir(), 'v3_probe_forced_header_${os.getpid()}')
		os.rmdir_all(dir) or {}
		os.mkdir_all(dir) or { panic(err) }
		defer {
			os.rmdir_all(dir) or {}
		}
		forced := os.join_path(dir, 'forced.h')
		os.write_file(forced, '#define V3_FORCED_PROBE_MACRO 1\n') or { panic(err) }
		macros, complete := cache_c_compiler_predefined_macros(['-include', forced], 'cc',
			pref.host_target(), 'c')
		assert complete
		// The forced header runs before the empty probe input, so its macros must not
		// be reported as part of the predefined baseline the input scanner starts from.
		assert 'V3_FORCED_PROBE_MACRO' !in macros
	}
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
