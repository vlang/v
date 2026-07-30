module c

import os
import v3.flat
import v3.pref
import v3.types

fn test_late_source_does_not_reemit_multiline_header_context() {
	header := '#if defined(HEADER_IMPL)\n' + 'typedef struct { int value; } header_value;\n' +
		'#endif'
	directives := [
		header,
		'#ifdef __APPLE__',
		'#define OBJC_HELPER 1',
		'#include "/tmp/helper.m"',
		'#undef OBJC_HELPER',
		'#endif',
	]
	emission := c_source_directive_emission(directives, map[string]bool{})

	assert 0 !in emission.emit_late
	for i in 1 .. directives.len {
		assert i in emission.emit_late
	}
	assert 3 in emission.skip_early
}

fn test_multiline_inlined_c_function_definition_is_collected() {
	mut g := FlatGen.new()
	g.collect_inlined_c_fns('bool qrcodegen_encodeText(const char *text, uint8_t tempBuffer[],\n' +
		'\tenum qrcodegen_Ecc ecl, bool boostEcl) {\n' + '\treturn text != 0 && boostEcl;\n' + '}')
	g.collect_inlined_c_fns('void declared_with_anon_param(\n' + '\tstruct { int value; } item);')

	assert 'qrcodegen_encodeText' in g.inlined_c_fns
	assert 'declared_with_anon_param' !in g.inlined_c_fns
}

fn test_preserved_header_trees_scan_shared_files_once() {
	root := os.join_path(os.vtmp_dir(), 'v3_preserved_headers_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	shared_header := os.join_path(root, 'shared.h')
	first := os.join_path(root, 'first.h')
	second := os.join_path(root, 'second.h')
	os.write_file(shared_header, 'int shared_header_fn(void);\n')!
	os.write_file(first, '#include "shared.h"\n')!
	os.write_file(second, '#include "shared.h"\n')!

	mut g := FlatGen.new()
	g.collect_preserved_header_file(first, [root])
	g.collect_preserved_header_file(second, [root])

	assert 'shared_header_fn' in g.inlined_c_declared_fns
	assert g.preserved_header_files_seen.len == 3
}

fn test_large_nested_angle_header_stays_an_include() {
	root := os.join_path(os.vtmp_dir(), 'v3_nested_large_header_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	large_header := os.join_path(root, 'large.h')
	wrapper_header := os.join_path(root, 'wrapper.h')
	os.write_file(large_header, '#ifndef LARGE_H\n#define LARGE_H\n' + ' '.repeat(263_000) +
		'\nint large_header_fn(void);\n#endif\n')!
	os.write_file(wrapper_header, '#include <large.h>\nint wrapper_header_fn(void);\n')!

	header := c_inline_header_text('"${wrapper_header}"', '', wrapper_header, [root], false) or {
		panic('failed to inline wrapper header')
	}

	assert header.text.contains('#include <large.h>')
	assert !header.text.contains('int large_header_fn(void);')
	assert header.text.contains('int wrapper_header_fn(void);')
	assert header.preserved_headers.len == 1
	assert header.preserved_headers[0].include_arg == '<large.h>'
}

fn test_cache_tracks_omitted_native_function_definitions() {
	assert !c_cache_condition_is_negated_implementation_guard('!defined(FOO_IMPLEMENTATION) && FEATURE')
	assert !c_cache_condition_is_negated_implementation_guard('!!FOO_IMPLEMENTATION')
	mut g := FlatGen.new()
	g.cache_split = true
	g.collect_inlined_c_fns_for_cache('int native_source_fn(void) { return 1; }', true, false)
	g.collect_inlined_c_fns_for_cache('int native_header_fn(void) { return 2; }', false, true)
	g.collect_inlined_c_fns_for_cache('#ifdef FONTSTASH_IMPLEMENTATION\nint omitted_header_fn(void) { return 4; }\n#endif',
		false, true)
	g.collect_inlined_c_fns_for_cache('#ifndef FOO_IMPLEMENTATION\nint else_implementation_fn(void);\n#else\nint else_implementation_fn(void) { return 5; }\n#endif',
		false, true)
	g.collect_inlined_c_fns_for_cache('#if !defined(BAR_IMPLEMENTATION)\nint negated_guard_fn(void);\n#else\nint negated_guard_fn(void) { return 6; }\n#endif',
		false, true)
	g.collect_inlined_c_fns_for_cache('#if ( ! defined ( BAZ_IMPLEMENTATION ) )\nint spaced_negated_guard_fn(void);\n#else\nint spaced_negated_guard_fn(void) { return 7; }\n#endif',
		false, true)
	g.collect_inlined_c_fns_for_cache('static int native_static_fn(void) { return 3; }', false,
		true)

	assert 'native_source_fn' in g.cache_omitted_c_fns
	assert 'native_header_fn' !in g.cache_omitted_c_fns
	assert 'omitted_header_fn' in g.cache_omitted_c_fns
	assert 'else_implementation_fn' in g.cache_omitted_c_fns
	assert g.should_emit_c_extern_decl('else_implementation_fn')
	assert 'negated_guard_fn' in g.cache_omitted_c_fns
	assert g.should_emit_c_extern_decl('negated_guard_fn')
	assert 'spaced_negated_guard_fn' in g.cache_omitted_c_fns
	assert g.should_emit_c_extern_decl('spaced_negated_guard_fn')
	assert 'native_static_fn' in g.inlined_c_static_fns
}

fn test_cache_extern_declaration_avoids_tgmath_macro_expansion() {
	// A <tgmath.h> function-like macro can be pulled in by any build (e.g. gg's
	// Objective-C `gg_darwin.m`), so the parenthesized form is emitted regardless
	// of cache-split mode; only the listed math externs are affected.
	assert c_macro_safe_extern_decl('exp', 'double exp(double x);') == 'double (exp)(double x);'
	assert c_macro_safe_extern_decl('custom', 'int custom(int x);') == 'int custom(int x);'
}

fn test_cache_extern_filter_uses_pthread_preamble_declarations() {
	mut preamble_gen := FlatGen.new()
	preamble_gen.preamble()
	preamble := preamble_gen.sb.str()
	assert preamble.contains('int pthread_key_create(pthread_key_t* key, void (*dtor)(void*));')
	assert preamble.contains('void* pthread_getspecific(pthread_key_t key);')
	assert preamble.contains('int pthread_setspecific(pthread_key_t key, const void* const_ptr);')
	assert !preamble.contains('pthread_key_delete(')

	mut g := FlatGen.new()
	g.set_cache_split(true)

	assert !g.should_emit_c_extern_decl('pthread_key_create')
	assert !g.should_emit_c_extern_decl('pthread_getspecific')
	assert !g.should_emit_c_extern_decl('pthread_setspecific')
	assert g.should_emit_c_extern_decl('pthread_key_delete')
}

fn posix_declaration_filter_gen(target_os string, system_libc bool) FlatGen {
	mut ast := &flat.FlatAst{}
	mut tc := types.TypeChecker.new(ast)
	mut g := FlatGen.new()
	g.a = ast
	g.tc = &tc
	g.set_target(pref.target_from(target_os, 'amd64') or { panic(err) })
	if system_libc {
		g.add_c_directive('main', '#include <semaphore.h>', false)
	}
	return g
}

fn test_linux_family_system_libc_owns_itimerspec_and_semaphore_declarations() {
	for target_os in ['linux', 'android', 'termux'] {
		g := posix_declaration_filter_gen(target_os, true)
		assert g.c_directives_use_system_libc()
		assert g.skip_builtin_struct('C.itimerspec'), target_os
		for name in ['sem_destroy', 'sem_init', 'sem_post', 'sem_timedwait', 'sem_trywait',
			'sem_wait'] {
			assert !g.should_emit_c_extern_decl(name), '${target_os}: ${name}'
		}
	}
}

fn test_headerless_and_cross_target_keep_itimerspec_and_semaphore_declarations() {
	for target_os in ['linux', 'android', 'termux'] {
		headerless := posix_declaration_filter_gen(target_os, false)
		assert !headerless.c_directives_use_system_libc()
		assert !headerless.skip_builtin_struct('C.itimerspec'), target_os
		for name in ['sem_destroy', 'sem_init', 'sem_post', 'sem_timedwait', 'sem_trywait',
			'sem_wait'] {
			assert headerless.should_emit_c_extern_decl(name), '${target_os}: ${name}'
		}
	}

	cross_target := posix_declaration_filter_gen('freebsd', true)
	assert cross_target.c_directives_use_system_libc()
	assert !cross_target.skip_builtin_struct('C.itimerspec')
	for name in ['sem_destroy', 'sem_init', 'sem_post', 'sem_timedwait', 'sem_trywait', 'sem_wait'] {
		assert cross_target.should_emit_c_extern_decl(name), name
	}
}

fn test_builtin_abi_compat_macros_precede_late_c_source() {
	mut g := FlatGen.new()
	g.has_builtins = true
	g.add_c_directive('main', '#include "/tmp/helper.m"', false)
	g.preamble()
	g.emit_c_source_directives()
	code := g.sb.str()
	alias_pos := code.index('#define builtin__string_clone string__clone') or { -1 }
	source_pos := code.index('#include "/tmp/helper.m"') or { -1 }

	assert alias_pos >= 0
	assert source_pos > alias_pos
}
