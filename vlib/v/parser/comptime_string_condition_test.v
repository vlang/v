module parser

import os
import v.pref

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
