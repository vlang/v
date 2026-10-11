module parser

import os
import v.pref
import v.workers

fn test_parallel_unresolved_match_guards_do_not_publish_arm_constants() {
	root := os.join_path(os.vtmp_dir(), 'comptime_parallel_match_guards_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	contents := [
		"const flag = 'GET /users'.starts_with('GET')\n",
		"\$match flag { true { const chosen = 'yes' } \$else { const chosen = 'no' } }\n\$if chosen == 'yes' { fn selected() string { return 'yes' } } \$else { fn selected() string { return 'no' } }\n",
		'fn padding() {}\n',
		'fn main() { println(selected()) }\n',
	]
	mut paths := []string{}
	for i, content in contents {
		module_content := 'module main\n' + content
		path := os.join_path(root, '${i}.v')
		os.write_file(path, module_content + '\n'.repeat(40000 - module_content.len))!
		paths << path
	}
	for parallel in [false, true] {
		mut p := Parser.new(pref.new_preferences())
		if parallel { p.a.worker_pool = workers.new(3) }
		_, used_parallel := p.parse_files_dispatch(paths, parallel)
		assert used_parallel == parallel
		assert !p.diagnostics.any(it.severity == 'error:'), p.diagnostics.str()
		p.resolve_comptime_string_declarations()
		assert p.comptime_const_values[comptime_const_value_key('main', 'chosen')] == "'yes'"
		assert p.a.nodes.filter(it.kind == .fn_decl && it.value == 'selected').len == 1
	}
}

fn test_parallel_computed_constant_guards_preserve_chained_declarations() {
	root := os.join_path(os.vtmp_dir(), 'comptime_parallel_const_guards_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	contents := [
		"const route_has_get_method = 'GET /users'.starts_with('GET')\n",
		"\$if route_has_get_method { const chosen = 'yes' } \$else { const chosen = 'no' }\n",
		"\$if chosen == 'yes' { fn selected() string { return 'yes' } } \$else { fn selected() string { return 'no' } }\n",
		'fn main() { println(selected()) }\n',
	]
	mut paths := []string{}
	for i, content in contents {
		path := os.join_path(root, '${i}.v')
		module_content := 'module main\n' + content
		os.write_file(path, module_content + '\n'.repeat(40000 - module_content.len))!
		paths << path
	}
	for parallel in [false, true] {
		mut p := Parser.new(pref.new_preferences())
		if parallel { p.a.worker_pool = workers.new(3) }
		_, used_parallel := p.parse_files_dispatch(paths, parallel)
		assert used_parallel == parallel
		assert p.diagnostics.len == 0, p.diagnostics.str()
		p.resolve_comptime_string_declarations()
		assert p.comptime_const_values[comptime_const_value_key('main', 'chosen')] == "'yes'", p.comptime_const_values.str()
		assert p.a.nodes.filter(it.kind == .fn_decl && it.value == 'selected').len == 1
	}
}

fn test_parallel_unresolved_else_if_guards_preserve_following_declarations() {
	root := os.join_path(os.vtmp_dir(), 'comptime_parallel_else_if_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	contents := [
		"const first = 'GET /users'.starts_with('POST')\n",
		"\$if first { const chosen = 'wrong' } \$else \$if false { const inactive = 'unused' } \$else \$if true { const chosen = 'yes' } \$else { const dead = 'unused' }\nconst following = 'yes'\n",
		"\$if chosen == 'yes' && following == 'yes' { fn selected() string { return 'yes' } } \$else { fn selected() string { return 'no' } }\n",
		'fn main() { println(selected()) }\n',
	]
	mut paths := []string{}
	for i, content in contents {
		path := os.join_path(root, '${i}.v')
		module_content := 'module main\n' + content
		os.write_file(path, module_content + '\n'.repeat(40000 - module_content.len))!
		paths << path
	}
	for parallel in [false, true] {
		mut p := Parser.new(pref.new_preferences())
		if parallel { p.a.worker_pool = workers.new(3) }
		_, used_parallel := p.parse_files_dispatch(paths, parallel)
		assert used_parallel == parallel
		assert p.diagnostics.len == 0, p.diagnostics.str()
		p.resolve_comptime_string_declarations()
		assert p.comptime_const_values[comptime_const_value_key('main', 'chosen')] == "'yes'"
		assert p.comptime_const_values[comptime_const_value_key('main', 'following')] == "'yes'"
		assert !p.comptime_string_consts[comptime_const_value_key('main', 'inactive')]
		assert !p.comptime_string_consts[comptime_const_value_key('main', 'dead')]
	}
}

fn test_parallel_workers_preserve_unresolved_constant_names_for_later_batches() {
	root := os.join_path(os.vtmp_dir(), 'comptime_parallel_later_batch_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	mut paths := []string{}
	for i in 0 .. 4 {
		content := if i == 3 {
			"import config\nconst route_has_get_method = config.route.starts_with('GET')\n"
		} else {
			'fn padding_${i}() {}\n'
		}
		path := os.join_path(root, '${i}.v')
		module_content := 'module main\n' + content
		os.write_file(path, module_content + '\n'.repeat(40000 - module_content.len))!
		paths << path
	}
	mut p := Parser.new(pref.new_preferences())
	p.a.worker_pool = workers.new(3)
	_, used_parallel := p.parse_files_dispatch(paths, true)
	assert used_parallel
	assert p.comptime_string_consts[comptime_const_value_key('main', 'route_has_get_method')], p.comptime_string_consts.str()
	consumer := os.join_path(root, 'consumer.v')
	os.write_file(consumer, "module main\n\$if route_has_get_method { const chosen = 'yes' } \$else { const chosen = 'no' }\n")!
	p.parse_file(consumer)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	assert p.a.nodes.any(it.kind == .comptime_if)
	config := os.join_path(root, 'config.v')
	os.write_file(config, "module config\npub const route = 'GET /users'\n")!
	p.parse_file(config)
	p.resolve_comptime_string_declarations()
	assert p.comptime_const_values[comptime_const_value_key('main', 'chosen')] == "'yes'", p.comptime_const_values.str()
}

fn test_comptime_metadata_headers_do_not_leave_orphan_selectors() {
	path := os.join_path(os.vtmp_dir(), 'comptime_metadata_headers_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	os.write_file(path, 'fn reflect[T]() {\n\$for item in T.fields {}\n\$for item in T.methods {}\n\$for item in T.values {}\n\$for item in T.variants {}\n\$for item in T.attributes {}\n\$for item in T.params {}\n}\nfn main() {\n\$for word in "one two".fields() { println(word) }\n}\n')!
	mut p := Parser.new(pref.new_preferences())
	p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	loops := p.a.nodes.filter(it.kind == .comptime_for)
	assert loops.len == 7
	for loop in loops {
		if loop.value == 'word|strings' {
			assert loop.children_count == 2
			assert p.a.child_node(&loop, 1).kind == .call
		} else {
			assert loop.typ == 'T'
			assert loop.children_count == 1
		}
	}
	assert !p.a.nodes.any(it.kind == .selector && it.children_count == 1
		&& p.a.child_node(&it, 0).kind == .ident && p.a.child_node(&it, 0).value == 'T')
}

fn test_fn_literal_string_bindings_do_not_defer_outer_plain_comparisons() {
	path := os.join_path(os.vtmp_dir(), 'comptime_string_fn_literal_scope_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	os.write_file(path, "fn main() {\n f := fn () { x := 'inside'; _ = x }\n _ = f\n \$if x == 'inside' { println('wrong') } \$else { println('ok') }\n}\n")!
	mut p := Parser.new(pref.new_preferences())
	p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	assert !p.a.nodes.any(it.kind == .comptime_if)
	assert p.a.nodes.any(it.kind == .string_literal && it.value == 'ok')
	assert !p.a.nodes.any(it.kind == .string_literal && it.value == 'wrong')
}

fn test_function_literal_parameters_hide_and_restore_outer_static_values() {
	path := os.join_path(os.vtmp_dir(), 'comptime_string_closure_shadow_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	os.write_file(path, "fn main() {\n route := 'local'\n check := fn(route string) { \$if route.starts_with('loc') { println('wrong') } }\n check('runtime')\n \$if route.starts_with('loc') { println('restored') } \$else { println('lost') }\n}\n")!
	mut p := Parser.new(pref.new_preferences())
	p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	assert p.a.nodes.filter(it.kind == .comptime_if).len == 1
	assert p.a.nodes.any(it.kind == .string_literal && it.value == 'restored')
	assert !p.a.nodes.any(it.kind == .string_literal && it.value == 'lost')
}

fn test_comptime_loop_bindings_hide_and_restore_outer_static_values() {
	path := os.join_path(os.vtmp_dir(), 'comptime_string_loop_shadow_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	os.write_file(path, "fn main() {\n route := 'constant'\n \$for route in 'runtime'.fields() {\n  \$if route.starts_with('con') { println('wrong') }\n }\n \$if route.starts_with('con') { println('restored') } \$else { println('lost') }\n}\n")!
	mut p := Parser.new(pref.new_preferences())
	p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	assert p.a.nodes.filter(it.kind == .comptime_if).len == 1
	assert p.a.nodes.any(it.kind == .string_literal && it.value == 'restored')
	assert !p.a.nodes.any(it.kind == .string_literal && it.value == 'lost')
}

fn test_static_value_probe_preserves_deep_expression_recovery() {
	path := os.join_path(@VEXEROOT, 'vlib/v/parser/tests/check_undefined_variables_too_deep_nested.vv')
	mut compiler := Parser.new(pref.new_preferences())
	compiler.parse_file(path)
	assert compiler.diagnostics.any(it.message == 'expr level > 100'), compiler.diagnostics.str()
	mut prefs := pref.new_preferences()
	prefs.is_fmt = true
	mut formatter := Parser.new(prefs)
	formatter.parse_file(path)
	assert formatter.diagnostics.len == 0, formatter.diagnostics.str()
}

fn test_static_string_conditions_select_declarations_before_collection() {
	path := os.join_path(os.vtmp_dir(), 'comptime_string_declarations_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	os.write_file(path, "const route = 'GET /users/:id'\n\$if route.all_before(' ') == 'GET' && route[5..10] == 'users' {\n fn selected() int { return 1 }\n} \$else {\n fn selected() Unavailable { return unavailable() }\n}\n\$if 'ABC'.to_lower().ends_with('z') {\n fn absent() Unavailable { return unavailable() }\n} \$else {\n fn fallback() int { return 2 }\n}\n")!
	mut p := Parser.new(pref.new_preferences())
	p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut names := []string{}
	for node in p.a.nodes {
		if node.kind == .fn_decl { names << node.value }
		assert node.kind != .comptime_if
		assert node.typ != 'Unavailable'
	}
	assert names == ['selected', 'fallback'], names.str()
}

fn test_unknown_string_operations_stay_deferred() {
	path := os.join_path(os.vtmp_dir(), 'comptime_string_unknown_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	os.write_file(path, "fn main() { \$if 'abc'.repeat(1) == 'abc' { println('yes') } \$else { println('no') } }")!
	mut p := Parser.new(pref.new_preferences())
	p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	assert p.a.nodes.any(it.kind == .comptime_if)
}

fn test_qualified_string_constants_remain_deferred_when_not_yet_parsed() {
	path := os.join_path(os.vtmp_dir(), 'comptime_string_qualified_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	os.write_file(path, "import route_data\nfn main() { \$if route_data.route == 'module' { assert true } \$else { assert false } }")!
	mut p := Parser.new(pref.new_preferences())
	p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	conditions := p.a.nodes.filter(it.kind == .comptime_if)
	assert conditions.len == 1, conditions.str()
	assert conditions[0].value == "route_data.route == 'module'", conditions[0].value
}

fn test_unresolved_declaration_guards_do_not_publish_branch_constants() {
	path := os.join_path(os.vtmp_dir(), 'comptime_string_guard_scope_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	os.write_file(path, "\$if string is int { const selected = 'yes' } \$else { const selected = 'no' }\n\$if selected == 'yes' { fn chosen() {} } \$else { fn other() {} }\n")!
	mut p := Parser.new(pref.new_preferences())
	p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	p.resolve_comptime_string_declarations()
	assert p.comptime_value('selected') == none
	assert p.a.nodes.filter(it.kind == .comptime_if).len == 2
}

fn test_guarded_imported_constants_resolve_before_consumers() {
	root := os.join_path(os.vtmp_dir(), 'comptime_string_parser_guard_order_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	main_path := os.join_path(root, 'main.v')
	data_path := os.join_path(root, 'route_data.v')
	config_path := os.join_path(root, 'config.v')
	os.write_file(main_path, "import route_data as rd\n\$if rd.selected == 'yes' { const chosen = 'yes' } \$else { const chosen = 'no' }\n")!
	os.write_file(data_path, "module route_data\nimport config as cfg\n\$if cfg.enabled { pub const selected = 'yes' } \$else { pub const selected = 'no' }\n")!
	os.write_file(config_path, 'module config\npub const enabled = true\n')!
	mut p := Parser.new(pref.new_preferences())
	p.parse_file(main_path)
	p.parse_file(data_path)
	p.parse_file(config_path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	p.resolve_comptime_string_declarations()
	assert p.comptime_const_values[comptime_const_value_key('main', 'chosen')] == "'yes'", p.comptime_const_values.str()
	assert p.a.nodes.all(it.kind != .comptime_if), p.a.nodes.filter(it.kind == .comptime_if).str()
}

fn test_static_string_probes_preserve_shadowing_local_bindings() {
	root := os.join_path(os.vtmp_dir(), 'comptime_string_shadow_scope_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	for body in [
		"fn check(route string) { \$if route.starts_with('con') { println('wrong') } }",
		"fn main() { mut route := 'runtime'; \$if route.starts_with('con') { println('wrong') } }",
		"struct Values { route string }\nfn main() { main := Values{route: 'runtime'}; \$if main.route.starts_with('con') { println('wrong') } }",
	] {
		path := os.join_path(root, 'main.v')
		os.write_file(path, "const route = 'constant'\n${body}\n")!
		mut p := Parser.new(pref.new_preferences())
		p.parse_file(path)
		assert p.diagnostics.len == 0, p.diagnostics.str()
		assert p.a.nodes.filter(it.kind == .comptime_if).len == 1
	}
	path := os.join_path(root, 'immutable.v')
	os.write_file(path, "const route = 'constant'\nfn main() { route := 'local'; \$if route.starts_with('loc') { println('selected') } \$else { println('wrong') } }\n")!
	mut p := Parser.new(pref.new_preferences())
	p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	assert p.a.nodes.all(it.kind != .comptime_if)
	assert p.a.nodes.any(it.kind == .string_literal && it.value == 'selected')
	assert !p.a.nodes.any(it.kind == .string_literal && it.value == 'wrong')
}

fn test_unresolved_scalar_import_guards_defer_only_their_imports() {
	root := os.join_path(os.vtmp_dir(), 'comptime_string_deferred_imports_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'main.v')
	os.write_file(path, 'import config as cfg\n\$if cfg.enabled {\n import inactive\n}\n\$if string is int {\n import type_guarded\n const enabled = cfg.enabled\n \$if enabled {\n  import nested_inactive\n }\n}\n\$if threads {\n import thread_guarded\n}\n')!
	mut p := Parser.new(pref.new_preferences())
	p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	deferred := p.resolve_comptime_string_declarations()
	assert deferred.len == 2, deferred.str()
	for id, _ in deferred {
		assert p.a.nodes[id].kind == .import_decl
		assert p.a.nodes[id].value in ['inactive', 'nested_inactive']
	}
	config_path := os.join_path(root, 'config.v')
	os.write_file(config_path, 'module config\npub const enabled = false\n')!
	p.parse_file(config_path)
	assert p.resolve_comptime_string_declarations().len == 0
	assert p.a.nodes.any(it.kind == .import_decl && it.value == 'type_guarded')
	assert !p.a.nodes.any(it.kind == .import_decl && it.value == 'nested_inactive')
	assert comptime_const_value_key('main', 'enabled') !in p.comptime_const_values
}
