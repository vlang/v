module c

import os

fn test_comptime_string_sources_emit_no_runtime_parsing_or_array() {
	root := os.join_path(os.vtmp_dir(), 'comptime_string_sources_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source_path := os.join_path(@VEXEROOT, 'vlib/v/tests/comptime/comptime_string_sources_test.v')
	mut first_body := ''
	for flags in [([]string{}), ['-no-parallel']] {
		c_path := os.join_path(root, if flags.len == 0 { 'parallel.c' } else { 'serial.c' })
		mut args := [@VEXE, '-nocache', '-gc', 'none', '-o', c_path]
		args << flags
		args << source_path
		result := os.exec(args)
		assert result.exit_code == 0, result.output
		code := os.read_file(c_path)!
		signatures := code.split_into_lines().filter(it.contains(' static_string_dispatch(') && it.ends_with(' {'))
		assert signatures.len == 1, signatures.str()
		body := code.all_after(signatures[0] + '\n').all_before('\n}')
		assert body.len > 0
		if first_body.len > 0 {
			assert body == first_body
		} else {
			first_body = body
		}
		for call in ['string__all_before(', 'string__all_after(', 'string__trim_left(', 'string__count(',
			'string__split(', 'string__starts_with(', 'new_array', 'malloc(', 'v_malloc('] {
			assert !body.contains(call), '${call}: ${body}'
		}
		assert body.contains('StringSourceApp__user_posts('), body
		assert body.contains('StringSourceApp__create_user('), body
		assert code.contains('void _vinit() {')
		init := code.all_after('void _vinit() {').all_before('\n}')
		assert !init.contains('string__all_after('), init
		assert !init.contains('string__trim_left('), init
		assert !init.contains('string__to_lower('), init
		assert !init.contains('string__substr('), init
	}
}

fn test_comptime_string_sources_reject_runtime_or_unsupported_operands() {
	root := os.join_path(os.vtmp_dir(), 'comptime_string_sources_invalid_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	cases := {
		'mutable_source':        "fn main() { mut path := 'a/b'; path = 'c/d'; \$for part in path.split('/') { println(part) } }"
		'runtime_source':        "fn walk(path string) { \$for part in path.split('/') { println(part) } }\nfn main() { walk('a/b') }"
		'unsupported_source':    "fn main() { \$for part in 'abc'.bytes() { println(part) } }"
		'mutable_condition':     "fn main() { mut path := 'a'; path = 'b'; \$if path.starts_with('a') { println('bad') } }"
		'overflow_slice':        "fn main() { \$for part in 'abc'[4294967296..].fields() { println(part) } }"
		'overflow_high_slice':   "fn main() { \$for part in 'abc'[..4294967296].fields() { println(part) } }"
		'overflow_condition':    "fn main() { \$if 'abc'[4294967296..].starts_with('a') { println('bad') } }"
		'unsupported_condition': "fn main() { \$if 'abc'.repeat(1) == 'abc' { println('bad') } }"
	}
	for name, source in cases {
		path := os.join_path(root, '${name}.v')
		os.write_file(path, source)!
		result := os.exec([@VEXE, '-nocache', '-o', os.join_path(root, '${name}.c'), path])
		assert result.exit_code != 0, '${name}: ${result.output}'
		if name == 'mutable_condition' {
			assert result.output.contains('`path` is mut and may have changed since its definition'), result.output
		} else if name in ['unsupported_condition', 'overflow_condition'] {
			assert result.output.contains('cannot evaluate `\$if` condition'), result.output
		} else {
			assert result.output.contains('on a compile-time-known string'), result.output
		}
	}
}

fn test_comptime_string_constant_operands_keep_their_module_scope() {
	root := os.join_path(os.vtmp_dir(), 'comptime_string_constant_scope_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'route_data'))!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), 'Module { name: "string_scope" }')!
	os.write_file(os.join_path(root, 'route_data/route_data.v'), "module route_data\nconst private_prefix = 'GET /module'\npub const route = private_prefix.all_after(' ').trim_left('/')\npub const yes = route.starts_with('mod')\n")!
	path := os.join_path(root, 'main.v')
	os.write_file(path, "import route_data\nconst prefix = 'POST /main'\nconst enabled = route_data.yes\n\$if route_data.yes { const direct = true } \$else { const direct = false }\n\$if enabled { const selected = 'yes' } \$else { const selected = 'no' }\nfn main() {\n prefix := 'DELETE /local'\n assert prefix.all_after(' ') == '/local'\n assert route_data.route == 'module'\n assert selected == 'yes'\n assert direct\n \$if selected == 'yes' { assert true } \$else { assert false }\n \$if route_data.route == 'module' { assert true } \$else { assert false }\n \$for word in route_data.route.fields() { assert word == 'module' }\n}")!
	result := os.exec([@VEXE, '-nocache', '-gc', 'none', '-o', os.join_path(root, 'scope'), path])
	assert result.exit_code == 0, result.output
	run := os.exec([os.join_path(root, 'scope')])
	assert run.exit_code == 0, run.output
	alias_source := os.read_file(path)!.replace('import route_data', 'import route_data as rd').replace('route_data.', 'rd.')
	os.write_file(path, alias_source)!
	alias_result := os.exec([@VEXE, '-nocache', '-gc', 'none', '-o', os.join_path(root, 'alias_scope'),
		path])
	assert alias_result.exit_code == 0, alias_result.output
	alias_run := os.exec([os.join_path(root, 'alias_scope')])
	assert alias_run.exit_code == 0, alias_run.output
}

fn test_static_string_conditions_discard_unavailable_top_level_declarations() {
	root := os.join_path(os.vtmp_dir(), 'comptime_string_top_level_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'main.v')
	os.write_file(path, "const route = 'GET /users'.all_after(' ').trim_left('/')\n\$if route.starts_with('users') {\n fn selected() int { return 1 }\n} \$else {\n fn selected() Unavailable { return unavailable() }\n}\n\$if 'ABC'.to_lower().starts_with('z') {\n fn fallback() Unavailable { return unavailable() }\n} \$else {\n fn fallback() int { return 2 }\n}\nfn main() { assert selected() == 1; assert fallback() == 2 }\n")!
	result := os.exec([@VEXE, '-nocache', '-gc', 'none', '-o', os.join_path(root, 'selected'),
		path])
	assert result.exit_code == 0, result.output
	run := os.exec([os.join_path(root, 'selected')])
	assert run.exit_code == 0, run.output
}

fn test_imported_guarded_constants_resolve_before_consumer_declarations() {
	root := os.join_path(os.vtmp_dir(), 'comptime_string_guard_order_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'route_data'))!
	os.mkdir_all(os.join_path(root, 'config'))!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), 'Module { name: "guard_order" }')!
	os.write_file(os.join_path(root, 'config/config.v'), 'module config\npub const enabled = true\n')!
	os.write_file(os.join_path(root, 'route_data/route_data.v'), "module route_data\nimport config as cfg\n\$if cfg.enabled { pub const selected = 'yes' } \$else { pub const selected = 'no' }\n")!
	path := os.join_path(root, 'main.v')
	os.write_file(path, "import route_data as rd\n\$if rd.selected == 'yes' {\n const chosen = 'yes'\n fn selected() int { return 1 }\n} \$else {\n const chosen = 'no'\n fn selected() Unavailable { return unavailable() }\n}\nfn main() { assert chosen == 'yes'; assert selected() == 1 }\n")!
	result := os.exec([@VEXE, '-nocache', '-gc', 'none', '-o', os.join_path(root, 'guard_order'),
		path])
	assert result.exit_code == 0, result.output
	run := os.exec([os.join_path(root, 'guard_order')])
	assert run.exit_code == 0, run.output
}

fn test_immutable_local_string_sources_in_normal_function_blocks() {
	root := os.join_path(os.vtmp_dir(), 'comptime_string_local_source_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'main.v')
	os.write_file(path, "fn main() {\n path := 'GET /users/:id/posts'.all_after(' ').trim_left('/')\n \$for segment in path.split('/') {\n \$if !segment.starts_with(':') { println(segment) }\n }\n}\n")!
	result := os.exec([@VEXE, '-nocache', '-gc', 'none', '-o', os.join_path(root, 'local'), path])
	assert result.exit_code == 0, result.output
	run := os.exec([os.join_path(root, 'local')])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'users\nposts', run.output
}
