module c

import os
import v.flat
import v.pref
import v.types

fn test_late_source_does_not_reemit_multiline_header_context() {
	header := '#if defined(HEADER_IMPL)\n' + 'typedef struct { int value; } header_value;\n' + '#endif'
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
	g.collect_inlined_c_fns('bool qrcodegen_encodeText(const char *text, uint8_t tempBuffer[],\n' + '\tenum qrcodegen_Ecc ecl, bool boostEcl) {\n' + '\treturn text != 0 && boostEcl;\n' + '}')
	g.collect_inlined_c_fns('void declared_with_anon_param(\n' + '\tstruct { int value; } item);')

	assert 'qrcodegen_encodeText' in g.inlined_c_fns
	assert 'declared_with_anon_param' !in g.inlined_c_fns
}

fn test_header_backed_declarations_do_not_get_a_second_prototype() {
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

	// A postincluded header is emitted after every declaration and call site, so it
	// cannot be the declaration they use and the prototype has to stay.
	mut postinclude_g := FlatGen.new()
	postinclude_g.collect_c_directive('main', flat.Node{
		kind:  .directive
		value: 'postinclude'
		typ:   '"${header}"'
	}, source, false)
	assert 'postinclude_api' !in postinclude_g.inlined_c_declared_fns
	assert '#include "${header}"' in postinclude_g.postinclude_directives
	assert postinclude_g.should_emit_c_extern_decl_from_file('postinclude_api', source, 'main')

	// A preincluded header comes first, so it owns what it declares.
	mut preinclude_g := FlatGen.new()
	preinclude_g.collect_c_directive('main', flat.Node{
		kind:  .directive
		value: 'preinclude'
		typ:   '"${header}"'
	}, source, false)
	assert 'postinclude_api' !in preinclude_g.inlined_c_declared_fns
	assert !preinclude_g.should_emit_c_extern_decl_from_file('postinclude_api', source, 'main')
	assert '#include "${header}"' in preinclude_g.preinclude_directives
}

fn test_include_preserves_header_without_scanning_declarations() {
	root := os.join_path(os.vtmp_dir(), 'v3_unscanned_header_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'main.v')
	header := os.join_path(root, 'api.h')
	os.write_file(source, 'fn main() {}\n')!
	os.write_file(header, 'typedef struct api_type api_type;\nint header_api(void);\n')!

	mut g := FlatGen.new()
	g.collect_c_directive('main', flat.Node{
		kind:  .directive
		value: 'include'
		typ:   '"${header}"'
	}, source, false)

	assert g.c_directives.len == 1
	assert g.c_directives[0].text == '#include "${header}"'
	assert 'api_type' !in g.inlined_c_typedef_names
	assert 'header_api' !in g.inlined_c_declared_fns
	// The header is not scanned, so V3 cannot tell `header_api` from `unrelated_api`.
	// It declares neither: whatever the file includes owns both names.
	assert !g.should_emit_c_extern_decl_from_file('unrelated_api', source, 'main')
	assert !g.should_emit_c_extern_decl_from_file('header_api', source, 'main')

	// The same declaration in a file that links a C object instead keeps its prototype.
	mut linked := FlatGen.new()
	linked.note_c_flag_directive('main', source, '@VMODROOT/api.o')
	assert linked.should_emit_c_extern_decl_from_file('header_api', source, 'main')
}

fn test_preinclude_scans_macro_state_without_scanning_declarations() {
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
	os.write_file(api_header, '#ifdef ENABLE_CHAINED_API\n#define chained_api(x) ((x) + 1)\n#endif\nint chained_decl(void);\n')!
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
	assert 'chained_decl' !in g.inlined_c_declared_fns
	assert !g.should_emit_c_extern_decl_from_file('chained_decl', source, 'main')
	assert g.preinclude_directives == ['#include "${config_header}"', '#include "${api_header}"']
}

fn test_forced_include_flags_scan_macro_state() {
	root := os.join_path(os.vtmp_dir(), 'v3_forced_include_macro_state_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	include_header := os.join_path(root, 'include.h')
	imacros_header := os.join_path(root, 'imacros.h')
	os.write_file(include_header, '#define forced_include_api(p) ((p)->value)\n#undef forced_ordered_api\n')!
	os.write_file(imacros_header, '#define forced_imacros_api(p) ((p)->value)\n#define forced_ordered_api(p) ((p)->value)\n')!

	mut g := FlatGen.new()
	g.c_flags = ['-include=${include_header}', '-imacros', imacros_header]
	g.collect_forced_include_active_macros()

	assert 'forced_include_api' in g.inlined_c_active_macros
	assert 'forced_imacros_api' in g.inlined_c_active_macros
	assert 'forced_ordered_api' !in g.inlined_c_active_macros

	mut unresolved := FlatGen.new()
	unresolved.c_flags = ['-include', os.join_path(root, 'missing.h')]
	unresolved.collect_forced_include_active_macros()
	assert unresolved.has_unscanned_forced_c_include
	assert unresolved.c_symbol_may_be_from_unscanned_header('C.unknown_api', 'unknown_api')
}

fn test_c_flag_macros_override_compiler_predefined_environment() {
	include_macros, dynamic_macros := c_flag_include_macro_definitions(['-U__GNUC__',
		'-DPROJECT_FEATURE=1'], {
		'__GNUC__': '14'
	})
	assert '__GNUC__' !in include_macros
	assert '__GNUC__' !in dynamic_macros
	assert dynamic_macros['PROJECT_FEATURE']
}

fn test_active_macro_environment_classifies_function_like_c_flags() {
	mut g := FlatGen.new()
	g.c_flags = ['-Dstrict_flag_api(p)=((p)->value)', '-D', 'split_flag_api(p)=((p)->value)',
		'-Dremoved_flag_api(p)=((p)->value)', '-Uremoved_flag_api']
	g.set_c_compiler_predefined_macros({
		'compiler_function_api': '(p) ((p)->value)'
		'compiler_object_api':   ' (1)'
	}, true)
	g.initialize_c_active_macro_environment()

	assert 'strict_flag_api' in g.inlined_c_active_macros
	assert 'split_flag_api' in g.inlined_c_active_macros
	assert 'compiler_function_api' in g.inlined_c_active_macros
	assert 'compiler_object_api' !in g.inlined_c_active_macros
	assert 'removed_flag_api' !in g.inlined_c_active_macros
}

fn test_consecutive_includes_share_macro_state() {
	root := os.join_path(os.vtmp_dir(), 'v3_ordered_include_macro_state_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	first_header := os.join_path(root, 'first.h')
	second_header := os.join_path(root, 'second.h')
	source := os.join_path(root, 'main.v')
	os.write_file(first_header, '#define ENABLE_ORDERED_API 1\n#define ordered_api(p) ((p)->value)\n')!
	os.write_file(second_header, '#if defined(ENABLE_ORDERED_API) && defined(ordered_api)\n#undef ordered_api\n#endif\n')!
	os.write_file(source, 'fn main() {}\n')!

	mut g := FlatGen.new()
	for header in [first_header, second_header] {
		g.collect_c_directive('main', flat.Node{
			kind:  .directive
			value: 'include'
			typ:   '"${header}"'
		}, source, false)
	}

	assert 'ordered_api' !in g.inlined_c_active_macros
}

fn test_pragma_once_header_is_not_replayed_after_undef() {
	root := os.join_path(os.vtmp_dir(), 'v3_pragma_once_macro_state_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	header := os.join_path(root, 'once.h')
	source := os.join_path(root, 'main.c.v')
	os.write_file(header, '#pragma once\n#define once_api(p) ((p)->value)\n')!

	mut g := FlatGen.new()
	g.collect_c_directive('main', flat.Node{
		kind:  .directive
		value: 'include'
		typ:   '"${header}"'
	}, source, false)
	g.collect_c_directive('main', flat.Node{
		kind:  .directive
		value: 'undef'
		typ:   'once_api'
	}, source, false)
	g.collect_c_directive('main', flat.Node{
		kind:  .directive
		value: 'include'
		typ:   '"${header}"'
	}, source, false)

	assert 'once_api' !in g.inlined_c_active_macros
}

fn test_include_next_activates_unscanned_header_fallback() {
	root := os.join_path(os.vtmp_dir(), 'v3_include_next_macro_state_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	wrapper := os.join_path(root, 'wrapper.h')
	source := os.join_path(root, 'main.c.v')
	os.write_file(wrapper, '#include_next <wrapped_api.h>\n')!

	mut g := FlatGen.new()
	g.collect_c_directive('main', flat.Node{
		kind:  .directive
		value: 'include'
		typ:   '"${wrapper}"'
	}, source, false)
	assert source in g.files_with_unscanned_c_includes

	mut forced := FlatGen.new()
	forced.collect_included_c_active_macros('"${wrapper}"', '', []string{}, false)
	assert forced.has_unscanned_forced_c_include
}

fn test_preincludes_are_scanned_before_ordinary_includes() {
	root := os.join_path(os.vtmp_dir(), 'v3_preinclude_macro_order_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	define_header := os.join_path(root, 'define.h')
	undef_header := os.join_path(root, 'undef.h')
	source := os.join_path(root, 'main.c.v')
	os.write_file(define_header, '#define reordered_api(p) ((p)->value)\n')!
	os.write_file(undef_header, '#undef reordered_api\n')!

	mut ast := &flat.FlatAst{}
	ast.nodes = [
		flat.Node{ kind: .file, value: source },
		flat.Node{ kind: .directive, value: 'include', typ: '"${define_header}"' },
		flat.Node{ kind: .directive, value: 'preinclude', typ: '"${undef_header}"' },
	]
	mut g := FlatGen.new()
	g.a = ast
	nodes := g.top_level_nodes()
	g.replay_c_active_macro_state(nodes)

	assert 'reordered_api' in g.inlined_c_active_macros
}

fn test_c_macros_are_replayed_in_emitted_module_order() {
	source := os.join_path(os.vtmp_dir(), 'macro_replay_main.c.v')
	dependency_source := os.join_path(os.vtmp_dir(), 'macro_replay_dependency.c.v')
	mut ast := &flat.FlatAst{}
	ast.nodes = [
		flat.Node{ kind: .file, value: source },
		flat.Node{ kind: .module_decl, value: 'main' },
		flat.Node{ kind: .import_decl, value: 'dependency' },
		flat.Node{ kind: .directive, value: 'undef', typ: 'ordered_module_api' },
		flat.Node{ kind: .file, value: dependency_source },
		flat.Node{ kind: .module_decl, value: 'dependency' },
		flat.Node{
			kind:  .directive
			value: 'define'
			typ:   'ordered_module_api(p) ((p)->value)'
		},
	]
	mut g := FlatGen.new()
	g.a = ast
	g.module_imports['main'] = ['dependency']
	g.replay_c_active_macro_state(g.top_level_nodes())

	assert 'ordered_module_api' !in g.inlined_c_active_macros
}

fn test_cross_guarded_include_macro_mutations_are_ambiguous() {
	root := os.join_path(os.vtmp_dir(), 'v3_cross_guarded_macro_state_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	define_header := os.join_path(root, 'define.h')
	undef_header := os.join_path(root, 'undef.h')
	source := os.join_path(root, 'main.c.v')
	os.write_file(define_header, '#define cross_guarded_api(p) ((p)->value)\n')!
	os.write_file(undef_header, '#undef cross_guarded_api\n')!

	mut ast := flat.FlatAst.new()
	ast.add_node(flat.Node{ kind: .file, value: source })
	ast.add_node(flat.Node{ kind: .directive, value: 'include', typ: '"${define_header}"' })
	guarded_include := ast.add_node(flat.Node{
		kind:  .directive
		value: 'include'
		typ:   '"${undef_header}"'
	})
	block_children_start := ast.children.len
	ast.children << guarded_include
	block := ast.add_node(flat.Node{
		kind:           .block
		children_start: i32(block_children_start)
		children_count: 1
	})
	if_children_start := ast.children.len
	ast.children << block
	ast.add_node(flat.Node{
		kind:           .comptime_if
		value:          'windows'
		children_start: i32(if_children_start)
		children_count: 1
	})

	mut g := FlatGen.new()
	g.a = &ast
	g.set_output_cross_c(true)
	g.index_cross_directive_guards()
	g.replay_c_active_macro_state(g.top_level_nodes())

	assert int(guarded_include) in g.cross_directive_guards
	assert 'cross_guarded_api' in g.inlined_c_active_macros
}

fn test_inlined_c_macro_tracking_uses_final_state() {
	mut g := FlatGen.new()
	text := '#define final_api(p) ((p)->value)\n#undef final_api\nstatic inline int final_api(int *p) { return *p; }\n'
	g.collect_inlined_c_declared_fns(text)
	g.replay_inlined_c_macro_state(text, false)

	assert 'final_api' !in g.inlined_c_active_macros
}

fn test_function_macro_status_propagates_through_aliases() {
	mut g := FlatGen.new()
	source := os.join_path(os.vtmp_dir(), 'macro_alias.c.v')
	for directive in [
		flat.Node{ kind: .directive, value: 'define', typ: 'alias_impl(p) ((p)->value)' },
		flat.Node{ kind: .directive, value: 'define', typ: 'alias_middle alias_impl' },
		flat.Node{ kind: .directive, value: 'define', typ: 'alias_api alias_middle' },
	] {
		g.collect_c_directive('main', directive, source, false)
	}
	assert 'alias_api' in g.inlined_c_active_macros

	g.collect_c_directive('main', flat.Node{
		kind:  .directive
		value: 'undef'
		typ:   'alias_impl'
	}, source, false)
	assert 'alias_api' !in g.inlined_c_active_macros

	mut forward := FlatGen.new()
	for directive in [
		flat.Node{ kind: .directive, value: 'ifdef', typ: '__GNUC__' },
		flat.Node{ kind: .directive, value: 'define', typ: 'forward_api forward_impl' },
		flat.Node{ kind: .directive, value: 'endif' },
		flat.Node{ kind: .directive, value: 'define', typ: 'forward_impl(p) ((p)->value)' },
	] {
		forward.collect_c_directive('main', directive, source, false)
	}
	assert 'forward_api' in forward.inlined_c_active_macros
}

fn test_header_macro_push_and_pop_restore_function_like_state() {
	root := os.join_path(os.vtmp_dir(), 'v3_macro_push_pop_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	header := os.join_path(root, 'push_pop.h')
	source := os.join_path(root, 'main.c.v')
	os.write_file(header, '#define restored_api(p) ((p)->value)\n#pragma push_macro("restored_api")\n#undef restored_api\n#pragma pop_macro("restored_api")\n')!

	mut g := FlatGen.new()
	g.collect_c_directive('main', flat.Node{
		kind:  .directive
		value: 'include'
		typ:   '"${header}"'
	}, source, false)

	assert 'restored_api' in g.inlined_c_active_macros

	mut direct := FlatGen.new()
	for directive in [
		flat.Node{ kind: .directive, value: 'define', typ: 'direct_restored_api(p) ((p)->value)' },
		flat.Node{ kind: .directive, value: 'pragma', typ: 'push_macro("direct_restored_api")' },
		flat.Node{ kind: .directive, value: 'undef', typ: 'direct_restored_api' },
		flat.Node{ kind: .directive, value: 'pragma', typ: 'pop_macro("direct_restored_api")' },
	] {
		direct.collect_c_directive('main', directive, source, false)
	}
	assert 'direct_restored_api' in direct.inlined_c_active_macros
}

fn test_nested_include_is_scanned_or_marked_unresolved() {
	root := os.join_path(os.vtmp_dir(), 'v3_macro_expanded_include_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'main.c.v')
	nested_header := os.join_path(root, 'nested.h')
	wrapper_header := os.join_path(root, 'wrapper.h')
	os.write_file(nested_header, '#define nested_impl(p) ((p)->value)\n#define nested_api nested_impl\n')!
	os.write_file(wrapper_header, '#define NESTED_HEADER "nested.h"\n#include NESTED_HEADER\n')!

	mut g := FlatGen.new()
	g.collect_c_directive('main', flat.Node{
		kind:  .directive
		value: 'include'
		typ:   '"${wrapper_header}"'
	}, source, false)
	assert 'nested_api' in g.inlined_c_active_macros
	assert source !in g.files_with_unscanned_c_includes

	missing_header := os.join_path(root, 'missing_wrapper.h')
	os.write_file(missing_header, '#define MISSING_HEADER "missing.h"\n#include MISSING_HEADER\n')!
	mut unresolved := FlatGen.new()
	unresolved.collect_c_directive('main', flat.Node{
		kind:  .directive
		value: 'include'
		typ:   '"${missing_header}"'
	}, source, false)
	assert source in unresolved.files_with_unscanned_c_includes

	computed_wrapper := os.join_path(root, 'computed_wrapper.h')
	os.write_file(computed_wrapper, '#include PICK(api.h)\n')!
	mut computed_angle := FlatGen.new()
	computed_angle.c_flags = ['-I', root]
	computed_angle.collect_c_directive('main', flat.Node{
		kind:  .directive
		value: 'include'
		typ:   '<computed_wrapper.h>'
	}, source, false)
	assert source in computed_angle.files_with_unscanned_c_includes

	// Some compiler search mechanisms, such as macOS framework roots, are not
	// modeled by CGen. A missing literal include therefore needs the same fallback.
	literal_wrapper := os.join_path(root, 'literal_wrapper.h')
	os.write_file(literal_wrapper, '#include <MissingFramework/MissingFramework.h>\n')!
	mut literal_angle := FlatGen.new()
	literal_angle.c_flags = ['-I', root]
	literal_angle.collect_c_directive('main', flat.Node{
		kind:  .directive
		value: 'include'
		typ:   '<literal_wrapper.h>'
	}, source, false)
	assert source in literal_angle.files_with_unscanned_c_includes
}

fn test_compiler_include_search_output_is_parsed() {
	root := os.join_path(os.vtmp_dir(), 'v3_compiler_include_dirs_${os.getpid()}')
	first := os.join_path(root, 'first')
	second := os.join_path(root, 'second')
	os.rmdir_all(root) or {}
	os.mkdir_all(first)!
	os.mkdir_all(second)!
	defer {
		os.rmdir_all(root) or {}
	}

	gnu_output := '#include <...> search starts here:\n ${first}\n ${second} (framework directory)\nEnd of search list.\n'
	assert c_compiler_include_dirs_from_output(gnu_output) == [os.real_path(first)]
	tcc_output := 'install: /tmp/tcc\ninclude:\n  ${first}\n  ${second}\nlibraries:\n  /tmp/lib\n'
	assert c_compiler_include_dirs_from_output(tcc_output) == [os.real_path(first),
		os.real_path(second)]
}

fn test_compiler_default_include_paths_are_scanned() {
	$if macos {
		mut g := FlatGen.new()
		g.ccompiler = 'clang'
		source := os.join_path(os.vtmp_dir(), 'compiler_default_include.c.v')
		g.collect_c_directive('main', flat.Node{
			kind:  .directive
			value: 'include'
			typ:   '<stdatomic.h>'
		}, source, false)

		assert 'atomic_load' in g.inlined_c_active_macros
		// Darwin's wrapper reaches the implementation through `#include_next`;
		// preserving the fallback is conservative when that continuation is opaque.
		assert source in g.files_with_unscanned_c_includes
	}
}

fn test_active_macro_scan_uses_complete_compiler_macro_environment() {
	root := os.join_path(os.vtmp_dir(), 'v3_compiler_macro_state_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	header := os.join_path(root, 'compiler_selected.h')
	source := os.join_path(root, 'main.c.v')
	os.write_file(header, '#ifndef __GNUC__\n#define compiler_selected_api(p) ((p)->value)\n#endif\n')!

	mut gcc := FlatGen.new()
	gcc.set_c_compiler_predefined_macros({
		'__GNUC__': '14'
	}, true)
	gcc.initialize_c_active_macro_environment()
	gcc.collect_c_directive('main', flat.Node{
		kind:  .directive
		value: 'include'
		typ:   '"${header}"'
	}, source, false)
	assert 'compiler_selected_api' !in gcc.inlined_c_active_macros

	mut unknown := FlatGen.new()
	unknown.initialize_c_active_macro_environment()
	unknown.collect_c_directive('main', flat.Node{
		kind:  .directive
		value: 'include'
		typ:   '"${header}"'
	}, source, false)
	assert 'compiler_selected_api' in unknown.inlined_c_active_macros
}

fn test_active_macro_scan_evaluates_compiler_macro_values() {
	root := os.join_path(os.vtmp_dir(), 'v3_compiler_macro_value_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	header := os.join_path(root, 'version_selected.h')
	source := os.join_path(root, 'main.c.v')
	os.write_file(header, '#if __STDC_VERSION__ >= 201112L\n#else\n#define version_selected_api(p) ((p)->value)\n#endif\n')!

	mut modern := FlatGen.new()
	modern.set_c_compiler_predefined_macros({
		'__STDC_VERSION__': ' 201710L'
	}, true)
	modern.initialize_c_active_macro_environment()
	modern.collect_c_directive('main', flat.Node{
		kind:  .directive
		value: 'include'
		typ:   '"${header}"'
	}, source, false)
	assert 'version_selected_api' !in modern.inlined_c_active_macros

	mut legacy := FlatGen.new()
	legacy.set_c_compiler_predefined_macros({
		'__STDC_VERSION__': ' 199901L'
	}, true)
	legacy.initialize_c_active_macro_environment()
	legacy.collect_c_directive('main', flat.Node{
		kind:  .directive
		value: 'include'
		typ:   '"${header}"'
	}, source, false)
	assert 'version_selected_api' in legacy.inlined_c_active_macros
}

fn test_unresolved_header_fallback_is_scoped_to_its_c_declarations() {
	mut g := FlatGen.new()
	g.ccompiler = 'compiler-that-does-not-exist'
	source := os.join_path(os.vtmp_dir(), 'unresolved_default_include.c.v')
	g.collect_c_directive('main', flat.Node{
		kind:  .directive
		value: 'include'
		typ:   '<v3_missing_default_header.h>'
	}, source, false)
	g.note_c_fn_decl_source('possibly_a_macro', source)
	g.note_c_fn_decl_source('known_elsewhere', '/tmp/known_elsewhere.c.v')

	assert g.c_symbol_may_be_from_unscanned_header('C.possibly_a_macro', 'possibly_a_macro')
	assert !g.c_symbol_may_be_from_unscanned_header('C.known_elsewhere', 'known_elsewhere')
	g.c_extern_forced_decls['definitely_a_function'] = true
	g.note_c_fn_decl_source('definitely_a_function', source)
	assert !g.c_symbol_may_be_from_unscanned_header('C.definitely_a_function', 'definitely_a_function')
}

fn test_direct_macro_tracking_honors_conditionals() {
	mut g := FlatGen.new()
	source := os.join_path(os.vtmp_dir(), 'direct_macro_conditionals.c.v')
	for directive in [
		flat.Node{ kind: .directive, value: 'define', typ: 'kept_macro(p) ((p)->x)' },
		flat.Node{ kind: .directive, value: 'if', typ: '0' },
		flat.Node{ kind: .directive, value: 'undef', typ: 'kept_macro' },
		flat.Node{ kind: .directive, value: 'define', typ: 'inactive_macro(p) ((p)->x)' },
		flat.Node{ kind: .directive, value: 'endif' },
		flat.Node{ kind: .directive, value: 'if', typ: '1' },
		flat.Node{ kind: .directive, value: 'define', typ: 'active_macro(p) ((p)->x)' },
		flat.Node{ kind: .directive, value: 'endif' },
	] {
		g.collect_c_directive('main', directive, source, false)
	}

	assert 'kept_macro' in g.inlined_c_active_macros
	assert 'inactive_macro' !in g.inlined_c_active_macros
	assert 'active_macro' in g.inlined_c_active_macros
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
	untracked := c_collect_external_input_tree(entry, '', [root], mut active_paths, mut collected_paths, mut ambiguous_collected_paths, mut files, mut include_macros, mut dynamic_include_macros, mut literal_include_macros, mut resolution_dirs, mut missing_resolution_paths, mut active_static_storage_paths, mut captured_input_digests, 'main', ambient_ambiguous, false)
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
	os.write_file(os.join_path(root, 'right.h'), '#include "shared.h"\nint right_diamond_fn(void);\n')!
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
	os.write_file(os.join_path(root, 'left.h'), '#ifdef V3_UNKNOWN_TOGGLE\n#include "shared.h"\n#endif\nint left_ambiguous_fn(void);\n')!
	os.write_file(os.join_path(root, 'right.h'), '#include "shared.h"\nint right_ambiguous_fn(void);\n')!
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
	os.write_file(os.join_path(root, 'a.h'), '#ifndef A_H\n#define A_H\n#ifndef SKIP_EXTRA\n#include "extra.h"\n#endif\n#endif\n')!
	// The first include skips extra.h (SKIP_EXTRA defined). The root then undefines the
	// header guard and SKIP_EXTRA and includes a.h again: the real preprocessor traverses
	// it a second time and now selects extra.h, which the scanner must collect too.
	top := os.join_path(root, 'top.h')
	os.write_file(top, '#define SKIP_EXTRA\n#include "a.h"\n#undef A_H\n#undef SKIP_EXTRA\n#include "a.h"\n')!

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
	os.write_file(os.join_path(root, 'a.h'), '#ifndef A_H\n#define A_H\n#ifndef SKIP_EXTRA\n#include "extra.h"\n#endif\n#endif\n')!
	// The guard and selection macro are undefined under an unresolved `#if`, so their
	// defined state becomes ambiguous (`dynamic_include_macros[NAME] == false`) rather
	// than definitely defined. The preprocessor may still traverse a.h a second time and
	// select extra.h, so a membership-only guard test would wrongly skip and omit it.
	top := os.join_path(root, 'top.h')
	os.write_file(top, '#define SKIP_EXTRA\n#include "a.h"\n#if V3_UNKNOWN_TOGGLE\n#undef A_H\n#undef SKIP_EXTRA\n#endif\n#include "a.h"\n')!

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
	assert c_include_arg_is_builtin_abi_helper('"/root/vlib/sync/stdatomic/tcc_compat_aliases.h"', root)
	assert c_include_arg_is_builtin_abi_helper('"/root/vlib/sync/stdatomic/stdatomic_include_after_compat.h"', root)
	assert c_include_arg_is_builtin_abi_helper('"/root/thirdparty/stdatomic/nix/atomic.h"', root)
	assert c_include_arg_is_builtin_abi_helper('"C:\\root\\thirdparty\\stdatomic\\win\\atomic.h"', 'C:\\root')
	// A trailing slash on VROOT resolves to the same anchored path.
	assert c_include_arg_is_builtin_abi_helper('"/root/vlib/builtin/prealloc_atomics.h"', '/root/')
	// When VROOT is unknown the unexpanded pseudo-path still identifies the helper.
	assert c_include_arg_is_builtin_abi_helper('"@VEXEROOT/vlib/builtin/prealloc_atomics.h"', '')

	// An unrelated absolute user header that merely ends in a helper-shaped suffix,
	// but lives outside the active VROOT, keeps its declarations.
	assert !c_include_arg_is_builtin_abi_helper('"/tmp/vlib/os/filelock/filelock_helpers.h"', root)
	// The same header under a different VROOT is likewise not the active helper.
	assert !c_include_arg_is_builtin_abi_helper('"/root/vlib/os/filelock/filelock_helpers.h"', '/other')

	// A user header that merely shares a basename must not be dropped, even when the
	// basename is exactly one of V's helper headers.
	assert !c_include_arg_is_builtin_abi_helper('"src/filelock_helpers.h"', root)
	assert !c_include_arg_is_builtin_abi_helper('"prealloc_atomics.h"', root)
	assert !c_include_arg_is_builtin_abi_helper('"my_stdatomic_wrapper.h"', root)
	assert !c_include_arg_is_builtin_abi_helper('"vendor/atomic.h"', root)
	// The `/` boundary keeps a `.../myvlib/...` path from matching `/vlib/...`.
	assert !c_include_arg_is_builtin_abi_helper('"/home/user/myvlib/os/filelock/filelock_helpers.h"', root)
	// The real system header is not one of the superseded inline helpers either.
	assert !c_include_arg_is_builtin_abi_helper('<stdatomic.h>', root)
}

fn test_cache_tracks_omitted_native_function_definitions() {
	assert !c_cache_condition_is_negated_implementation_guard('!defined(FOO_IMPLEMENTATION) && FEATURE')
	assert !c_cache_condition_is_negated_implementation_guard('!!FOO_IMPLEMENTATION')
	mut g := FlatGen.new()
	g.cache_split = true
	g.collect_inlined_c_fns_for_cache('int native_source_fn(void) { return 1; }', true, false)
	g.collect_inlined_c_fns_for_cache('int native_header_fn(void) { return 2; }', false, true)
	g.collect_inlined_c_fns_for_cache('#ifdef FONTSTASH_IMPLEMENTATION\nint omitted_header_fn(void) { return 4; }\n#endif', false, true)
	g.collect_inlined_c_fns_for_cache('#ifndef FOO_IMPLEMENTATION\nint else_implementation_fn(void);\n#else\nint else_implementation_fn(void) { return 5; }\n#endif', false, true)
	g.collect_inlined_c_fns_for_cache('#if !defined(BAR_IMPLEMENTATION)\nint negated_guard_fn(void);\n#else\nint negated_guard_fn(void) { return 6; }\n#endif', false, true)
	g.collect_inlined_c_fns_for_cache('#if ( ! defined ( BAZ_IMPLEMENTATION ) )\nint spaced_negated_guard_fn(void);\n#else\nint spaced_negated_guard_fn(void) { return 7; }\n#endif', false, true)
	g.collect_inlined_c_fns_for_cache('static int native_static_fn(void) { return 3; }', false, true)
	g.collect_inlined_c_fns_for_cache('static int static_source_fn(void) { return 8; }', true, false)

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
		for name in ['sem_destroy', 'sem_init', 'sem_post', 'sem_timedwait', 'sem_trywait', 'sem_wait'] {
			assert !g.should_emit_c_extern_decl(name), '${target_os}: ${name}'
		}
	}
}

fn test_headerless_and_cross_target_keep_itimerspec_and_semaphore_declarations() {
	for target_os in ['linux', 'android', 'termux'] {
		headerless := posix_declaration_filter_gen(target_os, false)
		assert !headerless.c_directives_use_system_libc()
		assert !headerless.skip_builtin_struct('C.itimerspec'), target_os
		for name in ['sem_destroy', 'sem_init', 'sem_post', 'sem_timedwait', 'sem_trywait', 'sem_wait'] {
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
	dir := os.join_path(os.vtmp_dir(), 'v3_c_struct_source_owner_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	header_backed_file := os.join_path(dir, 'header_backed.v')
	headerless_file := os.join_path(dir, 'headerless.v')
	os.write_file(header_backed_file, 'module main\n#include "types.h"\n')!
	os.write_file(headerless_file, 'module main\n')!

	mut ast := &flat.FlatAst{}
	mut tc := types.TypeChecker.new(ast)
	mut g := FlatGen.new()
	g.a = ast
	g.tc = &tc
	g.register_struct_decl_info('C.NSFont', 'C.NSFont', 'uiold', 'ui_darwin.c.v', flat.Node{})
	assert g.skip_builtin_struct('C.NSFont')

	g.register_struct_decl_info('C.HeaderOwned', 'C.HeaderOwned', 'main', header_backed_file, flat.Node{})
	assert g.skip_builtin_struct('C.HeaderOwned')

	g.register_struct_decl_info('C.Local', 'C.Local', 'main', headerless_file, flat.Node{})
	assert !g.skip_builtin_struct('C.Local')

	g.register_struct_decl_info('C.Alias', 'C.Alias', 'main', headerless_file, flat.Node{})
	tc.c_typedef_structs['C.Alias'] = true
	assert g.skip_builtin_struct('C.Alias')
}

fn test_top_level_include_deduplication_resets_after_preprocessor_state_changes() {
	macro_directives := dedupe_top_level_c_includes(['#include <types.h>', '#include <types.h>',
		'#define FEATURE 1', '#include <types.h>'])
	assert macro_directives == ['#include <types.h>', '#define FEATURE 1', '#include <types.h>']

	conditional_directives := dedupe_top_level_c_includes(['#include <types.h>', '#if FEATURE',
		'#include <types.h>', '#endif', '#include <types.h>'])
	assert conditional_directives == ['#include <types.h>', '#if FEATURE', '#include <types.h>',
		'#endif', '#include <types.h>']
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
	commented, still_in_comment := c_preprocessor_directive_scan_line('  #define HIDDEN 1', in_comment)
	assert commented == ''
	assert still_in_comment
	visible, comment_ended := c_preprocessor_directive_scan_line('*/ #define VISIBLE "//" // tail', still_in_comment)
	assert visible == '#define VISIBLE "//"'
	assert !comment_ended
	trailing, _ := c_preprocessor_directive_scan_line('#if defined(ENABLED) // explanation', false)
	assert trailing == '#if defined(ENABLED)'
	not_a_directive, _ := c_preprocessor_directive_scan_line('int other; #define LATE 1', false)
	assert not_a_directive == ''
}

// A `#include linux <x.h>` is dropped when building for another target, so it must
// not make the file look header backed there and swallow the prototype of a
// declaration that only a linked C source can supply.
fn test_target_inactive_include_does_not_claim_header_ownership() {
	root := os.join_path(os.vtmp_dir(), 'v3_inactive_include_ownership_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'fn main() {}\n')!
	inactive_target := if os.user_os() == 'windows' { 'linux' } else { 'windows' }

	mut g := FlatGen.new()
	g.set_target(pref.target_from(os.user_os(), 'amd64') or { panic(err) })
	g.note_c_flag_directive('main', source, '@VMODROOT/helper.o')
	for kind in ['include', 'preinclude'] {
		g.collect_c_directive('main', flat.Node{
			kind:  .directive
			value: kind
			typ:   '${inactive_target} <ownership_probe.h>'
		}, source, false)
	}
	assert source !in g.files_with_c_includes
	assert 'main' !in g.mods_with_c_includes
	assert g.should_emit_c_extern_decl_from_file('helper_fn', source, 'main')

	// The same include for the target being built does claim ownership.
	mut active_g := FlatGen.new()
	active_g.set_target(pref.target_from(os.user_os(), 'amd64') or { panic(err) })
	active_g.note_c_flag_directive('main', source, '@VMODROOT/helper.o')
	active_g.collect_c_directive('main', flat.Node{
		kind:  .directive
		value: 'include'
		typ:   '${os.user_os()} <ownership_probe.h>'
	}, source, false)
	assert source in active_g.files_with_c_includes
	assert !active_g.should_emit_c_extern_decl_from_file('helper_fn', source, 'main')
}
