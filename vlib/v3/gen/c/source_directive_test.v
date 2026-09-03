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

fn test_postinclude_header_does_not_suppress_earlier_c_prototype() {
	root := os.join_path(os.vtmp_dir(), 'v3_postinclude_prototype_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	header := os.join_path(root, 'api.h')
	source := os.join_path(root, 'main.v')
	os.write_file(header, 'int postinclude_api(void);\n')!
	os.write_file(source, 'fn main() {}\n')!

	mut postinclude_g := FlatGen.new()
	postinclude_g.collect_c_directive('main', flat.Node{
		kind:  .directive
		value: 'postinclude'
		typ:   '"${header}"'
	}, source, false)
	assert 'postinclude_api' !in postinclude_g.inlined_c_declared_fns
	assert '#include "${header}"' in postinclude_g.postinclude_directives
	assert postinclude_g.should_emit_c_extern_decl_from_file('postinclude_api', source)

	mut preinclude_g := FlatGen.new()
	preinclude_g.collect_c_directive('main', flat.Node{
		kind:  .directive
		value: 'preinclude'
		typ:   '"${header}"'
	}, source, false)
	assert 'postinclude_api' in preinclude_g.inlined_c_declared_fns
}

fn test_unscanned_preserved_header_only_suppresses_known_symbols() {
	root := os.join_path(os.vtmp_dir(), 'v3_unscanned_header_${os.getpid()}')
	source := os.join_path(root, 'main.v')
	missing_header := os.join_path(root, 'compiler-search-only', 'api.h')

	mut g := FlatGen.new()
	g.collect_c_directive('main', flat.Node{
		kind:  .directive
		value: 'include'
		typ:   '"${missing_header}"'
	}, source, false)

	assert g.should_emit_c_extern_decl_from_file('unrelated_api', source)
	g.collect_preserved_c_fns(['header_api'])
	assert !g.should_emit_c_extern_decl_from_file('header_api', source)
	assert g.should_emit_c_extern_decl_from_file('unrelated_api', os.join_path(root, 'other.v'))
}

fn test_preinclude_carries_macro_state_to_later_preincludes() {
	root := os.join_path(os.vtmp_dir(), 'v3_preinclude_macro_state_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	config_header := os.join_path(root, 'config.h')
	api_header := os.join_path(root, 'api.h')
	source := os.join_path(root, 'main.v')
	os.write_file(config_header, '#define ENABLE_CHAINED_API 1\n')!
	os.write_file(api_header,
		'#ifdef ENABLE_CHAINED_API\n#define chained_api(x) ((x) + 1)\n#endif\n')!
	os.write_file(source, 'fn main() {}\n')!

	mut g := FlatGen.new()
	g.collect_c_directive('main', flat.Node{
		kind:  .directive
		value: 'preinclude'
		typ:   '"${config_header}"'
	}, source, false)
	g.collect_c_directive('main', flat.Node{
		kind:  .directive
		value: 'preinclude'
		typ:   '"${api_header}"'
	}, source, false)

	assert 'chained_api' in g.inlined_c_active_macros
	assert g.preinclude_directives == ['#include "${config_header}"', '#include "${api_header}"']
}

fn test_preserved_header_collects_only_definitely_active_declarations() {
	root := os.join_path(os.vtmp_dir(), 'v3_preserved_inactive_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	inactive_header := os.join_path(root, 'inactive.h')
	macro_header := os.join_path(root, 'macro.h')
	header := os.join_path(root, 'top.h')
	os.write_file(inactive_header, 'int nested_inactive_fn(void);\n')!
	os.write_file(macro_header,
		'#if defined(PARENT_HEADER_FEATURE)\nint parent_enabled_fn(void);\n#endif\n#define PRESERVED_HEADER_FEATURE 1\n')!
	os.write_file(header,
		'#ifdef OPTIONAL_API\nint optional_fn(void);\n#endif\n#if 0\nint inactive_fn(void);\n#include "inactive.h"\n#else\nint active_fn(void);\n#endif\n#define PARENT_HEADER_FEATURE 1\n#include "macro.h"\n#if defined(PRESERVED_HEADER_FEATURE)\nint include_enabled_fn(void);\n#endif\n')!

	mut g := FlatGen.new()
	g.collect_preserved_header_file(header, [root])

	assert 'inactive_fn' !in g.inlined_c_declared_fns
	assert 'nested_inactive_fn' !in g.inlined_c_declared_fns
	assert 'active_fn' in g.inlined_c_declared_fns
	assert 'optional_fn' !in g.inlined_c_declared_fns
	assert 'include_enabled_fn' in g.inlined_c_declared_fns
	assert 'parent_enabled_fn' in g.inlined_c_declared_fns
	assert os.real_path(inactive_header) !in g.preserved_header_files_seen
	assert os.real_path(macro_header) in g.preserved_header_files_seen

	mut enabled_g := FlatGen.new()
	enabled_g.c_flags << '-DOPTIONAL_API'
	enabled_g.collect_preserved_header_file(header, [root])
	assert 'optional_fn' in enabled_g.inlined_c_declared_fns
}

fn test_preserved_header_carries_child_macros_into_parent_remainder() {
	root := os.join_path(os.vtmp_dir(), 'v3_preserved_child_macro_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	child := os.join_path(root, 'config.h')
	parent := os.join_path(root, 'parent.h')
	os.write_file(child, '#define ENABLE_API 1\n')!
	os.write_file(parent,
		'#include "config.h"\n#ifdef ENABLE_API\n#define enabled_api(x) ((x) + 1)\n#endif\n')!

	mut g := FlatGen.new()
	g.collect_preserved_header_file(parent, [root])

	assert 'enabled_api' in g.inlined_c_active_macros
}

fn test_preserved_unguarded_header_is_rescanned_under_new_macro_state() {
	root := os.join_path(os.vtmp_dir(), 'v3_preserved_unguarded_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	child := os.join_path(root, 'api.h')
	parent := os.join_path(root, 'parent.h')
	os.write_file(child, '#ifdef ENABLE_API\n#define enabled_api(x) ((x) + 1)\n#endif\n')!
	os.write_file(parent, '#include "api.h"\n#define ENABLE_API 1\n#include "api.h"\n')!

	mut g := FlatGen.new()
	g.collect_preserved_header_file(parent, [root])

	assert 'enabled_api' in g.inlined_c_active_macros
	child_prefix := os.real_path(child) + '\n'
	assert g.preserved_header_scan_results.keys().filter(it.starts_with(child_prefix)).len >= 2
}

fn test_preserved_header_passes_definite_parent_macro_state_to_children() {
	root := os.join_path(os.vtmp_dir(), 'v3_preserved_parent_macro_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	child := os.join_path(root, 'child.h')
	parent := os.join_path(root, 'parent.h')
	os.write_file(child,
		'#ifdef ENABLE_API\n#define enabled_api(x) ((x) + 1)\n#endif\n#ifndef OMIT_API\nint omitted_api(void);\n#endif\n')!
	os.write_file(parent, '#define ENABLE_API 1\n#define OMIT_API 1\n#include "child.h"\n')!

	mut g := FlatGen.new()
	g.collect_preserved_header_file(parent, [root])

	assert 'enabled_api' in g.inlined_c_active_macros
	assert 'omitted_api' !in g.inlined_c_declared_fns
}

fn test_preserved_header_applies_valued_flag_macro_to_conditionals() {
	root := os.join_path(os.vtmp_dir(), 'v3_preserved_conditional_macro_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	header := os.join_path(root, 'conditional.h')
	os.write_file(header,
		'#if FEATURE == 2\n#define HAS_FOO\n#endif\n#ifndef HAS_FOO\nint foo(void);\n#endif\n#define HAS_BAR\n#if FEATURE == 2\n#undef HAS_BAR\n#endif\n#ifndef HAS_BAR\nint bar(void);\n#endif\nint always_active(void);\n')!

	mut g := FlatGen.new()
	// The scanner should match the real preprocessor: FEATURE is 2, so HAS_FOO is
	// defined and HAS_BAR is undefined.
	g.c_flags << '-DFEATURE=2'
	g.collect_preserved_header_file(header, [root])

	assert 'foo' !in g.inlined_c_declared_fns
	assert 'bar' in g.inlined_c_declared_fns
	assert 'always_active' in g.inlined_c_declared_fns
}

fn test_preserved_header_guards_externs_for_possibly_active_function_macros() {
	root := os.join_path(os.vtmp_dir(), 'v3_preserved_possible_macro_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	header := os.join_path(root, 'api.h')
	os.write_file(header,
		'#ifdef __MSVC_ONLY__\n#define compiler_api(x) ((x) + 1)\n#endif\n#if 0\n#define inactive_api(x) (x)\n#endif\n')!

	mut g := FlatGen.new()
	g.collect_preserved_header_file_with_state(header, [root], CHeaderMacroState{
		defined:                  map[string]bool{}
		undefined:                map[string]bool{}
		uncertain:                map[string]bool{}
		external_macros_possible: true
	})

	assert 'compiler_api' !in g.inlined_c_declared_fns
	assert 'compiler_api' in g.possibly_active_c_macros
	assert g.should_emit_c_extern_decl('compiler_api')
	assert g.c_possibly_active_macro_extern_decl('compiler_api', 'int compiler_api(int x);') == '#ifndef compiler_api\nint compiler_api(int x);\n#endif'
	assert 'inactive_api' !in g.inlined_c_declared_fns
	assert 'inactive_api' !in g.possibly_active_c_macros
}

fn test_preserved_headers_track_final_macro_state_for_externs() {
	root := os.join_path(os.vtmp_dir(), 'v3_preserved_final_macro_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	first := os.join_path(root, 'first.h')
	second := os.join_path(root, 'second.h')
	os.write_file(first,
		'#define same_header_api(x) ((x) + 1)\n#undef same_header_api\n#define declared_api(x) ((x) + 2)\n#undef declared_api\nint declared_api(void);\n#define later_header_api(x) ((x) + 3)\n')!
	os.write_file(second, '#undef later_header_api\n')!

	mut g := FlatGen.new()
	state := g.collect_preserved_header_file_with_state(first, [root], CHeaderMacroState{})
	assert 'same_header_api' !in g.inlined_c_active_macros
	assert 'later_header_api' in g.inlined_c_active_macros
	assert 'declared_api' in g.inlined_c_declared_fns
	assert g.should_emit_c_extern_decl('same_header_api')
	assert !g.should_emit_c_extern_decl('later_header_api')
	assert !g.should_emit_c_extern_decl('declared_api')
	g.collect_preserved_header_file_with_state(second, [root], state)
	assert 'later_header_api' !in g.inlined_c_active_macros
	assert g.should_emit_c_extern_decl('later_header_api')
	assert !g.should_emit_c_extern_decl('declared_api')
}

fn test_preserved_header_scans_includes_in_possibly_active_branches_for_macros() {
	root := os.join_path(os.vtmp_dir(), 'v3_preserved_possible_include_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	child := os.join_path(root, 'compiler_api.h')
	parent := os.join_path(root, 'parent.h')
	os.write_file(child,
		'#define compiler_api(x) ((x) + 1)\nint conditionally_declared_api(void);\n')!
	os.write_file(parent,
		'#ifdef __GNUC__\n#include "compiler_api.h"\n#endif\nint always_declared_api(void);\n')!

	mut g := FlatGen.new()
	g.collect_preserved_header_file(parent, [root])

	assert 'compiler_api' !in g.inlined_c_declared_fns
	assert 'compiler_api' in g.possibly_active_c_macros
	assert 'conditionally_declared_api' !in g.inlined_c_declared_fns
	assert 'always_declared_api' in g.inlined_c_declared_fns
	assert os.real_path(child) in g.preserved_header_files_seen
}

fn collect_external_input_tree_status(root string, entry string, ambient_ambiguous bool) (bool, []string) {
	mut active_paths := map[string]bool{}
	mut collected_paths := map[string]bool{}
	mut ambiguous_collected_paths := map[string]bool{}
	mut files := []string{}
	mut include_macros := map[string][]string{}
	mut dynamic_include_macros := map[string]bool{}
	mut literal_include_macros := map[string][]string{}
	mut resolution_dirs := map[string]bool{}
	mut missing_resolution_paths := map[string]bool{}
	mut active_static_storage_paths := map[string]bool{}
	mut captured_input_digests := map[string]string{}
	untracked := c_collect_external_input_tree(entry, '', [root], mut active_paths, mut
		collected_paths, mut ambiguous_collected_paths, mut files, mut include_macros, mut
		dynamic_include_macros, mut literal_include_macros, mut resolution_dirs, mut
		missing_resolution_paths, mut active_static_storage_paths, mut captured_input_digests,
		'main', ambient_ambiguous, false)
	return untracked, files
}

fn test_diamond_guarded_reinclude_stays_cacheable() {
	root := os.join_path(os.vtmp_dir(), 'v3_diamond_guarded_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'shared.h'), '#pragma once\nint shared_diamond_fn(void);\n')!
	os.write_file(os.join_path(root, 'left.h'), '#include "shared.h"\nint left_diamond_fn(void);\n')!
	os.write_file(os.join_path(root, 'right.h'),
		'#include "shared.h"\nint right_diamond_fn(void);\n')!
	top := os.join_path(root, 'top.h')
	os.write_file(top, '#include "left.h"\n#include "right.h"\n')!

	untracked, files := collect_external_input_tree_status(root, top, false)
	// An ordinary diamond include of a whole-file-guarded header is fully resolved
	// statically, so it must keep the module cache enabled.
	assert !untracked
	// The shared header is still recorded exactly once despite both branches including it.
	shared_path := os.real_path(os.join_path(root, 'shared.h'))
	assert files.filter(it == shared_path).len == 1, files.str()
}

fn test_ambiguous_guarded_reinclude_disables_cache() {
	root := os.join_path(os.vtmp_dir(), 'v3_diamond_guarded_ambiguous_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'shared.h'), '#pragma once\nint shared_ambiguous_fn(void);\n')!
	// The first traversal reaches the guarded header through an uncertain macro branch,
	// so whether its guard actually defined leaves the repeat include indeterminate.
	os.write_file(os.join_path(root, 'left.h'),
		'#ifdef V3_UNKNOWN_TOGGLE\n#include "shared.h"\n#endif\nint left_ambiguous_fn(void);\n')!
	os.write_file(os.join_path(root, 'right.h'),
		'#include "shared.h"\nint right_ambiguous_fn(void);\n')!
	top := os.join_path(root, 'top.h')
	os.write_file(top, '#include "left.h"\n#include "right.h"\n')!

	untracked, _ := collect_external_input_tree_status(root, top, false)
	assert untracked
}

fn test_reinclude_after_undef_rescans_new_dependencies() {
	root := os.join_path(os.vtmp_dir(), 'v3_reinclude_undef_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'extra.h'), '#pragma once\nint extra_fn(void);\n')!
	// A whole-file-guarded header whose body pulls in extra.h unless SKIP_EXTRA is
	// defined. With SKIP_EXTRA defined the include is statically inactive.
	os.write_file(os.join_path(root, 'a.h'),
		'#ifndef A_H\n#define A_H\n#ifndef SKIP_EXTRA\n#include "extra.h"\n#endif\n#endif\n')!
	// The first include skips extra.h (SKIP_EXTRA defined). The root then undefines the
	// header guard and SKIP_EXTRA and includes a.h again: the real preprocessor traverses
	// it a second time and now selects extra.h, which the scanner must collect too.
	top := os.join_path(root, 'top.h')
	os.write_file(top,
		'#define SKIP_EXTRA\n#include "a.h"\n#undef A_H\n#undef SKIP_EXTRA\n#include "a.h"\n')!

	_, files := collect_external_input_tree_status(root, top, false)
	extra_path := os.real_path(os.join_path(root, 'extra.h'))
	assert extra_path in files, 'undef re-include did not rescan extra.h: ${files.str()}'
}

fn test_reinclude_after_ambiguous_undef_rescans_new_dependencies() {
	root := os.join_path(os.vtmp_dir(), 'v3_reinclude_ambiguous_undef_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'extra.h'), '#pragma once\nint extra_fn(void);\n')!
	os.write_file(os.join_path(root, 'a.h'),
		'#ifndef A_H\n#define A_H\n#ifndef SKIP_EXTRA\n#include "extra.h"\n#endif\n#endif\n')!
	// The guard and selection macro are undefined under an unresolved `#if`, so their
	// defined state becomes ambiguous (`dynamic_include_macros[NAME] == false`) rather
	// than definitely defined. The preprocessor may still traverse a.h a second time and
	// select extra.h, so a membership-only guard test would wrongly skip and omit it.
	top := os.join_path(root, 'top.h')
	os.write_file(top,
		'#define SKIP_EXTRA\n#include "a.h"\n#if V3_UNKNOWN_TOGGLE\n#undef A_H\n#undef SKIP_EXTRA\n#endif\n#include "a.h"\n')!

	_, files := collect_external_input_tree_status(root, top, false)
	extra_path := os.real_path(os.join_path(root, 'extra.h'))
	assert extra_path in files, 'ambiguous undef re-include did not rescan extra.h: ${files.str()}'
}

fn test_whole_file_guard_rejects_alternative_branches() {
	// A plain `#ifndef`/`#define` guard, `#pragma once`, and a guard with only nested
	// conditionals are whole-file guarded.
	assert (c_whole_file_guard_macro('#ifndef H_H\n#define H_H\nint h_fn(void);\n#endif\n') or {
		'?'
	}) == 'H_H'
	assert (c_whole_file_guard_macro('#pragma once\nint once_fn(void);\n') or { '?' }) == ''
	assert (c_whole_file_guard_macro('#ifndef H_H\n#define H_H\n#ifdef X\nint a(void);\n#else\nint b(void);\n#endif\n#endif\n') or {
		'?'
	}) == 'H_H'
	// A guard-level `#else` or `#elif` runs an alternative branch on a repeat include, so
	// the file is not whole-file guarded and must not be classified as one.
	if guard := c_whole_file_guard_macro('#ifndef H_H\n#define H_H\nint h_fn(void);\n#else\n#include "alt.h"\n#endif\n') {
		assert false, 'guard-level #else must not be whole-file guarded, got `${guard}`'
	}
	if guard := c_whole_file_guard_macro('#ifndef H_H\n#define H_H\n#elif defined(OTHER)\n#include "alt.h"\n#endif\n') {
		assert false, 'guard-level #elif must not be whole-file guarded, got `${guard}`'
	}
}

fn test_builtin_abi_helper_matches_only_exact_headers() {
	root := '/root'
	// The genuinely superseded helpers are matched only when they resolve under the
	// active VROOT (`@VEXEROOT/...` in the directive expands to `vroot` + suffix).
	assert c_include_arg_is_builtin_abi_helper('"/root/vlib/builtin/prealloc_atomics.h"', root)
	assert c_include_arg_is_builtin_abi_helper('"/root/vlib/os/filelock/filelock_helpers.h"', root)
	assert c_include_arg_is_builtin_abi_helper('"/root/vlib/sync/stdatomic/tcc_compat_aliases.h"',
		root)
	assert c_include_arg_is_builtin_abi_helper('"/root/vlib/sync/stdatomic/stdatomic_include_after_compat.h"',
		root)
	assert c_include_arg_is_builtin_abi_helper('"/root/thirdparty/stdatomic/nix/atomic.h"', root)
	assert c_include_arg_is_builtin_abi_helper('"C:\\root\\thirdparty\\stdatomic\\win\\atomic.h"',
		'C:\\root')
	// A trailing slash on VROOT resolves to the same anchored path.
	assert c_include_arg_is_builtin_abi_helper('"/root/vlib/builtin/prealloc_atomics.h"', '/root/')
	// When VROOT is unknown the unexpanded pseudo-path still identifies the helper.
	assert c_include_arg_is_builtin_abi_helper('"@VEXEROOT/vlib/builtin/prealloc_atomics.h"', '')

	// An unrelated absolute user header that merely ends in a helper-shaped suffix,
	// but lives outside the active VROOT, keeps its declarations.
	assert !c_include_arg_is_builtin_abi_helper('"/tmp/vlib/os/filelock/filelock_helpers.h"', root)
	// The same header under a different VROOT is likewise not the active helper.
	assert !c_include_arg_is_builtin_abi_helper('"/root/vlib/os/filelock/filelock_helpers.h"',
		'/other')

	// A user header that merely shares a basename must not be dropped, even when the
	// basename is exactly one of V's helper headers.
	assert !c_include_arg_is_builtin_abi_helper('"src/filelock_helpers.h"', root)
	assert !c_include_arg_is_builtin_abi_helper('"prealloc_atomics.h"', root)
	assert !c_include_arg_is_builtin_abi_helper('"my_stdatomic_wrapper.h"', root)
	assert !c_include_arg_is_builtin_abi_helper('"vendor/atomic.h"', root)
	// The `/` boundary keeps a `.../myvlib/...` path from matching `/vlib/...`.
	assert !c_include_arg_is_builtin_abi_helper('"/home/user/myvlib/os/filelock/filelock_helpers.h"',
		root)
	// The real system header is not one of the superseded inline helpers either.
	assert !c_include_arg_is_builtin_abi_helper('<stdatomic.h>', root)
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
	g.collect_inlined_c_fns_for_cache('static int static_source_fn(void) { return 8; }', true,
		false)

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
	assert 'static_source_fn' in g.cache_omitted_c_fns
	assert g.should_emit_c_extern_decl('static_source_fn')
	assert g.c_extern_decl_is_cached_object_fallback('static_source_fn')
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

fn test_cache_split_uses_system_sigaction_declaration() {
	mut g := posix_declaration_filter_gen('macos', false)
	g.set_cache_split(true)
	assert g.skip_builtin_struct('C.sigaction')
}

fn test_c_struct_declared_in_platform_binding_stays_header_owned() {
	mut ast := &flat.FlatAst{}
	mut tc := types.TypeChecker.new(ast)
	mut g := FlatGen.new()
	g.a = ast
	g.tc = &tc
	g.register_struct_decl_info('C.NSFont', 'C.NSFont', 'uiold', 'ui_darwin.c.v', flat.Node{})
	assert g.skip_builtin_struct('C.NSFont')

	g.register_struct_decl_info('C.Local', 'C.Local', 'main', 'main.v', flat.Node{})
	assert !g.skip_builtin_struct('C.Local')
}

fn test_headerless_preamble_keeps_explicit_puts_declaration() {
	mut headerless := FlatGen.new()
	assert !headerless.c_directives_use_system_libc()
	assert headerless.should_emit_c_extern_decl('puts')
	assert headerless.should_emit_c_extern_decl('sendfile')

	mut system_libc := FlatGen.new()
	system_libc.add_c_directive('main', '#include <stdio.h>', false)
	assert system_libc.c_directives_use_system_libc()
	assert !system_libc.should_emit_c_extern_decl('puts')
	assert !system_libc.should_emit_c_extern_decl('sendfile')
}

fn test_builtin_boehm_directives_use_system_libc() {
	mut boehm := FlatGen.new()
	boehm.add_c_directive('builtin', '#include <gc.h>', false)
	assert boehm.c_directives_use_system_libc()

	mut closure := FlatGen.new()
	closure.add_c_directive('closure', '#include <sys/mman.h>\n#include <pthread.h>', false)
	assert !closure.c_directives_use_system_libc()
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

fn test_preprocessor_scan_tracks_comments_after_source_code() {
	first, in_comment := c_preprocessor_directive_scan_line('int value; /* comment starts', false)
	assert first == ''
	assert in_comment
	commented, still_in_comment := c_preprocessor_directive_scan_line('  #define HIDDEN 1',
		in_comment)
	assert commented == ''
	assert still_in_comment
	visible, comment_ended := c_preprocessor_directive_scan_line('*/ #define VISIBLE "//" // tail',
		still_in_comment)
	assert visible == '#define VISIBLE "//"'
	assert !comment_ended
	trailing, _ := c_preprocessor_directive_scan_line('#if defined(ENABLED) // explanation', false)
	assert trailing == '#if defined(ENABLED)'
	not_a_directive, _ := c_preprocessor_directive_scan_line('int other; #define LATE 1', false)
	assert not_a_directive == ''
}
