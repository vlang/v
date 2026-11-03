module driver

import v.flat
import v.cmdexec
import v.gen.c as cgen
import v.modulecache
import v.pref
import os
import time

fn test_tcc_dependency_scan_tracks_native_helper_headers() {
	tcc := os.join_path(@VEXEROOT, 'thirdparty', 'tcc', 'tcc.exe')
	if !os.is_file(tcc) {
		return
	}
	if cmdexec.run(tcc, ['-v']).exit_code != 0 {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'tcc_native_dependencies_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	header := os.join_path(root, 'helper.h')
	source := os.join_path(root, 'helper.c')
	os.write_file(header, '#define HELPER_VALUE 42\n')!
	os.write_file(source, '#include "helper.h"\nint helper(void) { return HELPER_VALUE; }\n')!
	deps := c_object_dependencies(tcc, [], source)
	assert !deps.used_fallback
	assert source in deps.files
	assert header in deps.files
}

fn test_macos_tcc_accepts_generated_c_objects_but_rejects_foreign_objects() {
	root := os.join_path(os.vtmp_dir(), 'tcc_object_provenance_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'helper.c')
	request := os.join_path(root, 'helper.o')
	resolved := [os.join_path(root, 'cache', 'helper_content_hash.o')]
	os.write_file(source, 'int helper(void) { return 42; }\n')!
	assert !v3_tcc_link_has_incompatible_objects(resolved, [request], '/tools/tcc.exe')
	assert v3_tcc_link_has_incompatible_objects(resolved, [request], 'clang')
	assert v3_tcc_link_has_incompatible_objects(resolved, [], '/tools/tcc.exe')
	os.write_file(request, 'prebuilt object')!
	assert v3_tcc_link_has_incompatible_objects(resolved, [request], '/tools/tcc.exe')
}

fn native_inputs_of(a &flat.FlatAst, user_c_flags []string) cgen.CacheNativeInputs {
	return cgen.cache_native_inputs(a, @VEXEROOT, pref.host_target(), user_c_flags, map[string]string{},
		map[string]bool{})
}

fn test_user_native_inputs_require_a_compiler_dependency_manifest() {
	mut a := flat.FlatAst.new()
	assert native_inputs_of(&a, []string{}).user_supplied == ''
	assert native_inputs_of(&a, ['-include', 'native.h']).user_supplied == 'native.h'
	for directive in ['include', 'insert', 'preinclude', 'postinclude'] {
		mut included := flat.FlatAst.new()
		included.add_val(.file, '/project/main.v')
		included.add_node(flat.Node{ kind: .directive, value: directive, typ: flat.source_type_id('"native.h"') })
		assert native_inputs_of(&included, []string{}).user_supplied == '"native.h"'
	}
	for flags in [['-I', '/project/include'], ['-I/project/include'], ['-isystem/project/include'],
		['-include=/project/forced.h'], ['/project/native.o'], ['@/project/c_flags.rsp']] {
		assert native_inputs_of(&a, flags).user_supplied != '', flags.str()
	}
	mut flagged := flat.FlatAst.new()
	flagged.add_val(.file, '/project/main.v')
	flagged.add_node(flat.Node{ kind: .directive, value: 'flag', typ: flat.source_type_id('-I /project/include') })
	assert native_inputs_of(&flagged, []string{}).user_supplied == '/project/include'
	mut packaged := flat.FlatAst.new()
	packaged.add_val(.file, '/project/main.v')
	packaged.add_node(flat.Node{ kind: .directive, value: 'pkgconfig', typ: flat.source_type_id('sdl2') })
	assert native_inputs_of(&packaged, []string{}).user_supplied == '#pkgconfig sdl2'
}

fn test_shipped_and_system_native_inputs_keep_the_caches() {
	helper := os.real_path(os.join_path(@VEXEROOT, 'vlib', 'v', 'flat', 'flat_payload_helpers.h'))
	mut a := flat.FlatAst.new()
	a.add_val(.file, '/project/main.v')
	a.add_node(flat.Node{ kind: .directive, value: 'include', typ: flat.source_type_id('<stddef.h>') })
	a.add_node(flat.Node{ kind: .directive, value: 'include', typ: flat.source_type_id('"@VEXEROOT/vlib/v/flat/flat_payload_helpers.h"') })
	a.add_node(flat.Node{ kind: .directive, value: 'flag', typ: flat.source_type_id('-I @VEXEROOT/thirdparty/stdatomic/nix -lm -DV_NATIVE_TEST=1') })
	// A module shipped with V may leave a header to the C compiler's search path.
	a.add_val(.file, os.join_path(@VEXEROOT, 'vlib', 'os', 'os.c.v'))
	a.add_val(.module_decl, 'os')
	a.add_node(flat.Node{ kind: .directive, value: 'include', typ: flat.source_type_id('"not_shipped_system_header.h"') })
	a.add_node(flat.Node{ kind: .directive, value: 'flag', typ: flat.source_type_id('-I/opt/native/include') })
	inputs := native_inputs_of(&a, ['-g', '-O2', '-D GC_THREADS=1', '-lm'])
	assert inputs.user_supplied == ''
	assert inputs.module_inputs['main'] == [helper]
	assert 'os' !in inputs.module_inputs
}

fn test_parallel_c_generation_leaves_native_header_ownership_to_c_compiler() {
	prefix := '/* V3CACHE_NATIVE_DIRECTIVES_BEGIN */\n#include "not_present/native.h"\n/* V3CACHE_NATIVE_DIRECTIVES_END */\nvoid main__main(void);\n'
	header, safe := v3_parallel_c_declaration_header(prefix, @VEXEROOT)
	assert !safe
	assert header.contains('#include "not_present/native.h"')
	root := os.join_path(os.vtmp_dir(), 'parallel_user_header_${os.getpid()}_${time.now().unix_nano()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	user_header := os.join_path(root, 'native.h')
	os.write_file(user_header, 'static inline int native_value(void) { return 1; }\n')!
	user_prefix := '/* V3CACHE_NATIVE_DIRECTIVES_BEGIN */\n#include "${user_header}"\n/* V3CACHE_NATIVE_DIRECTIVES_END */\nvoid main__main(void);\n'
	_, user_safe := v3_parallel_c_declaration_header(user_prefix, @VEXEROOT)
	assert !user_safe
}

fn test_parallel_c_generation_keeps_shipped_headers_opaque() {
	helper := os.join_path(@VEXEROOT, 'vlib', 'v', 'flat', 'flat_payload_helpers.h')
	prefix := '/* V3CACHE_NATIVE_DIRECTIVES_BEGIN */\n#include "${helper}"\n#include <stdio.h>\n/* V3CACHE_NATIVE_DIRECTIVES_END */\nvoid main__main(void);\n'
	header, safe := v3_parallel_c_declaration_header(prefix, @VEXEROOT)
	assert !safe
	assert header.contains('#include "${helper}"')
	assert !header.contains('v_flat_payload_ptr_get')
	assert header.contains('#include <stdio.h>')
}

fn test_parallel_c_generation_keeps_segfault_handler_state_in_owner() ! {
	$if bsd || linux {
		helper := os.join_path(@VEXEROOT, 'vlib', 'builtin', 'segfault_handler_nix.h')
		prefix := '/* V3CACHE_NATIVE_DIRECTIVES_BEGIN */\n#include "${helper}"\n/* V3CACHE_NATIVE_DIRECTIVES_END */\n'
		header, safe := v3_parallel_c_declaration_header(prefix, @VEXEROOT)
		assert !safe
		assert header.contains('#include "${helper}"')
		root := os.join_path(os.vtmp_dir(), 'parallel_segfault_header_${os.getpid()}_${time.now().unix_nano()}')
		os.mkdir_all(root)!
		defer {
			os.rmdir_all(root) or {}
		}
		declarations := os.join_path(root, 'declarations.h')
		linkage_cleanup := '\n#if defined(V_SEGFAULT_HANDLER_INSTALL_LINKAGE) || defined(V_SEGFAULT_HANDLER_SCOPE)\n#error leaked installer linkage macro\n#endif\n'
		os.write_file(declarations, header + linkage_cleanup)!
		runtime_header := os.join_path(root, 'runtime.h')
		os.write_file(runtime_header, '#include "${helper}"\n' + linkage_cleanup)!
		// Preprocess the actual split header: a prototype is sufficient in body units,
		// while the saved signal actions and every handler helper belong to the owner.
		non_owner := cmdexec.run('cc', ['-E', '-P', '-x', 'c', '-DV_PARALLEL_CC=1', declarations])
		assert non_owner.exit_code == 0, non_owner.output
		assert non_owner.output.contains('void v_install_segfault_handler(void* fallback, void* main_argv);'), non_owner.output
		assert !non_owner.output.contains('void v_install_segfault_handler(void* fallback, void* main_argv) {'), non_owner.output
		for name in ['v_segfault_fallback', 'v_segfault_previous', 'v_segfault_previous_consumed',
			'v_segfault_signal_handler'] {
			assert !non_owner.output.contains(name), non_owner.output
		}
		owner := cmdexec.run('cc', ['-E', '-P', '-x', 'c', '-DV_PARALLEL_CC=1',
			'-DV_PARALLEL_CC_OUT_0=1', runtime_header])
		assert owner.exit_code == 0, owner.output
		assert owner.output.contains('void v_install_segfault_handler(void* fallback, void* main_argv) {'), owner.output
		assert !owner.output.contains('static void v_install_segfault_handler('), owner.output
		for name in ['v_segfault_fallback', 'v_segfault_previous', 'v_segfault_previous_consumed'] {
			assert owner.output.contains(name), owner.output
		}
		// Monolithic builds retain the existing internal linkage.
		monolithic := cmdexec.run('cc', ['-E', '-P', '-x', 'c', runtime_header])
		assert monolithic.exit_code == 0, monolithic.output
		assert monolithic.output.contains('static void v_install_segfault_handler('), monolithic.output
	}
}

fn test_parallel_c_generation_splits_with_the_segfault_handler() {
	helper := os.join_path(@VEXEROOT, 'vlib', 'builtin', 'segfault_handler_nix.h')
	prefix := '/* V3CACHE_NATIVE_DIRECTIVES_BEGIN */\n#include "${helper}"\n/* V3CACHE_NATIVE_DIRECTIVES_END */\n'
	header, safe := v3_parallel_c_declaration_header(prefix, @VEXEROOT)
	assert !safe
	assert header.contains('#include "${helper}"')
	assert !header.contains('void v_install_segfault_handler(void* fallback, void* main_argv);')
}

fn test_native_dependency_list_uses_compiler_manifest_without_reading_headers() {
	$if windows {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'native_manifest_${os.getpid()}_${time.now().unix_nano()}')
	os.mkdir_all(root)!
	header := os.join_path(root, 'native.h')
	nested := os.join_path(root, 'nested.h')
	os.write_file(header, '#include "nested.h"\n')!
	os.write_file(nested, '#define VALUE 3\n')!
	os.chmod(header, 0o000)!
	os.chmod(nested, 0o000)!
	defer {
		os.chmod(header, 0o644) or {}
		os.chmod(nested, 0o644) or {}
		os.rmdir_all(root) or { panic(err) }
	}
	compiler := os.join_path(root, 'cc')
	manifest := 'v3cache: ${os.quoted_path(header)} ${os.quoted_path(nested)}'
	os.write_file(compiler, '#!/bin/sh\nfor input do dependency_source="$input"; done\nprintf "%s \'%s\'\\n" ${os.quoted_path(manifest)} "$dependency_source"\n')!
	os.chmod(compiler, 0o755)!
	mut a := flat.FlatAst.new()
	a.add_val(.file, os.join_path(root, 'main.c.v'))
	a.add_node(flat.Node{ kind: .directive, value: 'include', typ: flat.source_type_id('"@DIR/native.h"') })
	prefs := pref.new_preferences()
	mut expected := [header, nested].map(os.real_path(it))
	expected.sort()
	assert native_build_input_paths(&a, prefs, []string{}, compiler) == expected
	empty := flat.FlatAst.new()
	assert native_build_input_paths(&empty, prefs, ['-include', header], compiler) == expected
}

fn test_cached_signal_header_has_one_owner_with_shared_program_declarations() ! {
	$if bsd || linux {
		header := os.join_path(@VEXEROOT, 'vlib', 'builtin', 'segfault_handler_nix.h')
		prefix := '/* V3CACHE_NATIVE_DIRECTIVES_BEGIN */\n#include "${header}"\n/* V3CACHE_NATIVE_DIRECTIVES_END */\n'
		owner_source := v3_cached_c_unit_source(prefix, true)
		declarations := v3_cached_c_unit_source(modulecache.declaration_header(prefix), false)
		body := 'void (*cached_installer(void))(void*, void*);\nint main(void) { return cached_installer() != v_install_segfault_handler; }\n'
		tcc_declarations := tcc_cached_main_source(declarations, body)
		root := os.join_path(os.vtmp_dir(), 'cached_signal_header_${os.getpid()}_${time.now().unix_nano()}')
		os.mkdir_all(root)!
		defer {
			os.rmdir_all(root) or {}
		}
		owner_path := os.join_path(root, 'owner.c')
		cached_path := os.join_path(root, 'cached.c')
		declarations_path := os.join_path(root, 'tcc_declarations.h')
		body_path := os.join_path(root, 'body.c')
		incremental_path := os.join_path(root, 'incremental.c')
		os.write_file(owner_path, owner_source)!
		os.write_file(cached_path, v3_cached_c_unit_source(prefix + 'void (*cached_installer(void))(void*, void*) { return v_install_segfault_handler; }\n',
			false))!
		os.write_file(declarations_path, tcc_declarations)!
		os.write_file(body_path, body)!
		os.write_file(incremental_path, v3_incremental_main_source(declarations_path, tcc_declarations, body))!
		// A cached combined source may later be split for parallel compilation.
		// The owner prelude copied into its declaration header cannot promote a body unit.
		parallel_header, safe := v3_parallel_c_declaration_header(owner_source,
			@VEXEROOT)
		assert !safe
		parallel_header_path := os.join_path(root, 'parallel.h')
		parallel_path := os.join_path(root, 'parallel.c')
		os.write_file(parallel_header_path, parallel_header)!
		os.write_file(parallel_path, v3_parallel_c_unit_source(parallel_header_path, body,
			false, false))!
		for path in [cached_path, declarations_path, incremental_path, parallel_path] {
			// An inherited owner flag must not turn cached declarations into definitions.
			flags := if path == parallel_path { []string{} } else { ['-DV_PARALLEL_CC_OUT_0=1'] }
			preprocessed := cmdexec.run('cc', ['-E', '-P', '-x', 'c', ...flags, path])
			assert preprocessed.exit_code == 0, preprocessed.output
			assert preprocessed.output.contains('void v_install_segfault_handler('), preprocessed.output
			for name in ['v_segfault_fallback', 'v_segfault_previous', 'v_segfault_previous_consumed',
				'v_segfault_signal_handler'] {
				assert !preprocessed.output.contains(name), '${path}: ${preprocessed.output}'
			}
			assert !preprocessed.output.contains('void v_install_segfault_handler(void* fallback, void* main_argv) {'), preprocessed.output
		}
		owner := cmdexec.run('cc', ['-E', '-P', '-x', 'c', owner_path])
		assert owner.exit_code == 0, owner.output
		assert owner.output.contains('void v_install_segfault_handler(void* fallback, void* main_argv) {'), owner.output
		assert !owner.output.contains('static void v_install_segfault_handler('), owner.output
		// Cached modules and incremental/TCC-style bodies resolve the same installer
		// from the physical program-prefix owner, without a second definition.
		binary := os.join_path(root, 'linked')
		linked := cmdexec.run('cc', ['-o', binary, owner_path, cached_path, incremental_path,
			...v3_default_linker_flags(pref.host_target().os, false)])
		assert linked.exit_code == 0, linked.output
		assert cmdexec.run(binary, []string{}).exit_code == 0
	}
}

fn test_only_declaration_headers_are_replicated_into_cached_objects() {
	assert modulecache.c_source_is_replicable('#include <stdio.h>\ntypedef struct { int x; } Foo;\nint foo_get(Foo *f);\nextern int foo_count;\nstatic inline int foo_one(void) { return 1; }\n')
	// A macro such as `GC_API` supplies the `extern` of a plain declaration, and
	// the linker merges weak definitions.
	assert modulecache.c_source_is_replicable('GC_API int GC_count;\n__attribute__ ((weak)) GC_API void GC_noop(void *p) { (void)p; }\n')
	assert !modulecache.c_source_is_replicable('int foo_get(void) { return 1; }\n')
	assert !modulecache.c_source_is_replicable('int foo_count = 0;\n')
	assert !modulecache.c_source_is_replicable('static int foo_count;\n')
	assert !modulecache.c_source_is_replicable('static inline int foo_next(void) {\n\tstatic int n;\n\treturn ++n;\n}\n')
	// Each cached object would get its own copy of a file-scope static object,
	// whatever its initializer or declarator looks like.
	assert !modulecache.c_source_is_replicable('static uint64_t foo_count = UINT64_C(0);\n')
	assert !modulecache.c_source_is_replicable('static int foo_count = FOO(1);\n')
	assert !modulecache.c_source_is_replicable('static int foo_size = sizeof(int);\n')
	assert !modulecache.c_source_is_replicable('static void (*foo_callback)(void);\n')
	assert !modulecache.c_source_is_replicable('static void (*foo_callback)(void) = 0;\n')
	assert !modulecache.c_source_is_replicable('static char foo_buf[sizeof(int)];\n')
	assert !modulecache.c_source_is_replicable('static int foo_counters[FOO(1)];\n')
	assert modulecache.c_source_is_replicable('static int foo_helper(int x);\n')
	assert modulecache.c_source_is_replicable('static int foo_helper(int a[sizeof(int)]);\n')
}

fn test_incremental_program_keeps_one_owner_for_cached_and_new_literals() ! {
	root := os.join_path(os.vtmp_dir(), 'incremental_literal_owner_${os.getpid()}_${time.now().unix_nano()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	declarations := 'typedef struct { const char* str; int len; int literal; } string;\nstatic const string _v3_lit_cached = {"cached", 6, 1};\nstatic inline int cached_length(void) { return _v3_lit_cached.len; }\n'
	declarations_path := os.join_path(root, 'declarations.h')
	os.write_file(declarations_path, declarations)!
	for length in [3, 7] {
		// Inline prefix code must keep its storage before the body, while each
		// edited body owns its new literal. The cached source remains reusable.
		body := 'static const string _v3_lit_cached = {"cached", 6, 1};\nstatic const string _v3_lit_new = {"new", ${length}, 1};\nint main(void) { return cached_length() != 6 || _v3_lit_cached.len != 6 || _v3_lit_new.len != ${length}; }\n'
		combined := v3_incremental_main_source(declarations_path, declarations, body)
		assert !combined.contains('static const string _v3_lit_cached =')
		assert combined.contains('static const string _v3_lit_new =')
		assert os.read_file(declarations_path)! == declarations
		source := os.join_path(root, 'main.c')
		binary := os.join_path(root, 'main')
		os.write_file(source, combined)!
		compiled := cmdexec.run('cc', ['-std=c11', '-Werror', '-o', binary, source])
		assert compiled.exit_code == 0, compiled.output
		assert cmdexec.run(binary, []string{}).exit_code == 0
	}
}

fn test_heap_guard_keeps_ordinary_module_caching_without_header_inspection() {
	mut a := flat.FlatAst.new()
	a.add_val(.file, os.join_path(@VEXEROOT, 'vlib', 'builtin', 'builtin.c.v'))
	a.add_val(.module_decl, 'builtin')
	a.add_node(flat.Node{ kind: .directive, value: 'insert', typ: flat.source_type_id('"@VEXEROOT/vlib/builtin/track_heap_checks.h"') })
	inputs := native_inputs_of(&a, []string{})
	guard := os.real_path(os.join_path(@VEXEROOT, 'vlib', 'builtin', 'track_heap_checks.h'))
	assert guard in inputs.module_inputs['builtin']
	assert !inputs.native_paths[guard]
	closure := v3_native_input_closure(&inputs)
	assert closure.unassignable == ''
	assert guard in closure.inputs['builtin']
}
