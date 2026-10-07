module c

import os

fn test_parallel_computed_constant_guards_select_the_same_runtime_branch() {
	root := os.join_path(os.vtmp_dir(), 'comptime_parallel_guards_codegen_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	for case in ['direct', 'chained', 'else_if', 'match'] {
		guard := if case == 'else_if' {
			"chosen == 'yes' && following == 'yes'"
		} else if case == 'chained' {
			"chosen == 'yes'"
		} else {
			'route_has_get_method'
		}
		contents := [
			if case == 'else_if' {
				"const route_has_get_method = 'GET /users'.starts_with('POST')\n"
			} else {
				"const route_has_get_method = 'GET /users'.starts_with('GET')\n"
			},
			if case == 'else_if' {
				"\$if route_has_get_method { const chosen = 'wrong' } \$else \$if false { const inactive = 'unused' } \$else \$if true { const chosen = 'yes' } \$else { const dead = 'unused' }\nconst following = 'yes'\n"
			} else if case == 'match' {
				"\$match route_has_get_method { true { const chosen = 'yes' } \$else { const chosen = 'no' } }\n\$if chosen == 'yes' { fn selected() string { return 'yes' } } \$else { fn selected() string { return 'no' } }\n"
			} else if case == 'chained' {
				"\$if route_has_get_method { const chosen = 'yes' } \$else { const chosen = 'no' }\n"
			} else {
				'fn padding() {}\n'
			},
			if case == 'match' {
				'fn padding() {}\n'
			} else {
				"\$if ${guard} { fn selected() string { return 'yes' } } \$else { fn selected() string { return 'no' } }\n"
			},
			'fn main() { println(selected()) }\n',
		]
		for i, content in contents {
			os.write_file(os.join_path(root, '${i}.v'), content + '\n'.repeat(40000 - content.len))!
		}
		for serial in [false, true] {
			binary := os.join_path(root, 'selected_${case}_${serial}')
			mut args := ['-new-compiler', '-no-retry-compilation', '-nocache', '-cc', 'clang',
				'-gc', 'none', '-o', binary]
			if serial { args << '-no-parallel' }
			args << root
			mut child := os.new_process(@VEXE)
			child.set_args(args)
			mut environment := os.environ()
			environment['VJOBS'] = '4'
			child.set_environment(environment)
			child.set_redirect_stdio_merged()
			child.run()
			output := child.stdout_slurp()
			child.wait()
			code := child.code
			child.close()
			assert code == 0, output
			result := os.exec([binary])
			assert result.exit_code == 0, result.output
			assert result.output.trim_space() == 'yes', '${case} ${serial}: ${result.output}'
		}
	}
}

fn comptime_router_body_with_literal_values(code string, body string) string {
	mut literals := map[string]string{}
	for line in code.split_into_lines() {
		if !line.starts_with('static const string _str_') { continue }
		name := line.all_after('static const string ').all_before(' = ')
		assert name !in literals, name
		literals[name] = line.all_after(' = ').trim_string_right(';')
	}
	mut resolved := ''
	mut i := 0
	for i < body.len {
		if body[i..].starts_with('_str_')
			&& (i == 0 || !(body[i - 1].is_letter() || body[i - 1].is_digit() || body[i - 1] == `_`)) {
			mut end := i + 5
			for end < body.len && body[end].is_digit() { end++ }
			if end > i + 5 {
				name := body[i..end]
				assert name in literals, name
				resolved += literals[name]
				i = end
				continue
			}
		}
		resolved += body[i].ascii_str()
		i++
	}
	return resolved
}

fn test_comptime_router_comparison_preserves_literal_contents() {
	first := comptime_router_body_with_literal_values('static const string _str_1 = {"users", 5, 1};',
		'string path = _str_1;')
	renumbered := comptime_router_body_with_literal_values('static const string _str_10 = {"users", 5, 1};',
		'string path = _str_10;')
	changed := comptime_router_body_with_literal_values('static const string _str_10 = {"posts", 5, 1};',
		'string path = _str_10;')
	assert first == renumbered
	assert first != changed
}

fn test_comptime_string_sources_emit_no_runtime_parsing_or_array() {
	root := os.join_path(os.vtmp_dir(), 'comptime_string_sources_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source_path := os.join_path(@VEXEROOT, 'vlib/v/tests/comptime/comptime_string_sources_test.v')
	mut first_body := ''
	for flags in [([]string{}), ['-no-parallel']] {
		c_path := os.join_path(root, if flags.len == 0 { 'parallel.c' } else { 'serial.c' })
		mut args := [@VEXE, '-new-compiler', '-no-retry-compilation', '-nocache', '-gc', 'none',
			'-o', c_path]
		args << flags
		args << source_path
		result := os.exec(args)
		assert result.exit_code == 0, result.output
		code := os.read_file(c_path)!
		signatures := code.split_into_lines().filter(it.contains(' static_string_dispatch(') && it.ends_with(' {'))
		assert signatures.len == 1, signatures.str()
		body := code.all_after(signatures[0] + '\n').all_before('\n}')
		assert body.len > 0
		// Global pool IDs depend on unrelated folded literals in other function workers.
		// Compare the complete literal initializers and control flow instead.
		resolved_body := comptime_router_body_with_literal_values(code, body)
		if first_body.len > 0 {
			assert resolved_body == first_body
		} else {
			first_body = resolved_body
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
		result := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-nocache', '-o',
			os.join_path(root, '${name}.c'), path])
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
	result := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-nocache', '-gc', 'none',
		'-o', os.join_path(root, 'scope'), path])
	assert result.exit_code == 0, result.output
	run := os.exec([os.join_path(root, 'scope')])
	assert run.exit_code == 0, run.output
	alias_source := os.read_file(path)!.replace('import route_data', 'import route_data as rd').replace('route_data.', 'rd.')
	os.write_file(path, alias_source)!
	alias_result := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-nocache', '-gc',
		'none', '-o', os.join_path(root, 'alias_scope'), path])
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
	result := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-nocache', '-gc', 'none',
		'-o', os.join_path(root, 'selected'), path])
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
	result := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-nocache', '-gc', 'none',
		'-o', os.join_path(root, 'guard_order'), path])
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
	result := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-nocache', '-gc', 'none',
		'-o', os.join_path(root, 'local'), path])
	assert result.exit_code == 0, result.output
	run := os.exec([os.join_path(root, 'local')])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'users\nposts', run.output
}

fn test_static_string_conditions_reject_runtime_shadows_of_constants() {
	root := os.join_path(os.vtmp_dir(), 'comptime_string_runtime_shadow_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	for body in [
		"fn check(route string) { \$if route.starts_with('con') { panic('wrong') } }\nfn main() { check('runtime') }",
		"fn main() { mut route := 'first'; route = 'second'; \$if route.starts_with('con') { panic('wrong') } }",
		"fn main() { route := 'constant'; check := fn(route string) { \$if route.starts_with('con') { panic('wrong') } }; check('runtime') }",
	] {
		path := os.join_path(root, 'main.v')
		os.write_file(path, "const route = 'constant'\n${body}\n")!
		result := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-nocache', '-gc',
			'none', '-o', os.join_path(root, 'shadow.c'), path])
		assert result.exit_code != 0, result.output
		assert !result.output.contains('C compilation error'), result.output
		assert result.output.contains('cannot evaluate `\$if` condition')
			|| result.output.contains('is mut and may have changed'), result.output
	}
	path := os.join_path(root, 'restored.v')
	os.write_file(path, "fn main() {\n route := 'constant'\n check := fn(route string) { assert route == 'runtime' }\n check('runtime')\n \$if route.starts_with('con') { assert true } \$else { assert false }\n}\n")!
	result := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-nocache', '-gc', 'none',
		'-o', os.join_path(root, 'restored'), path])
	assert result.exit_code == 0, result.output
	run := os.exec([os.join_path(root, 'restored')])
	assert run.exit_code == 0, run.output
}

fn test_scalar_import_guards_resolve_between_import_waves() {
	root := os.join_path(os.vtmp_dir(), 'comptime_string_import_waves_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'config'))!
	os.mkdir_all(os.join_path(root, 'broken'))!
	os.mkdir_all(os.join_path(root, 'route_data'))!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), 'Module { name: "import_waves" }')!
	data_source := 'module route_data\npub const yes = true\npub const value = 3\n'
	path := os.join_path(root, 'main.v')
	saved_no_file_index := os.getenv('V3_NO_FILE_IDX')
	defer { os.setenv('V3_NO_FILE_IDX', saved_no_file_index, true) }
	for mode in ['parallel', 'serial', 'full_scan'] {
		os.setenv('V3_NO_FILE_IDX', if mode == 'full_scan' { '1' } else { '' }, true)
		mut args := [@VEXE, '-new-compiler', '-no-retry-compilation', '-nocache', '-gc', 'none']
		if mode == 'serial' { args << '-no-parallel' }
		args << ['-o', os.join_path(root, 'selected'), path]
		os.write_file(os.join_path(root, 'route_data/route_data.v'), data_source)!
		os.write_file(os.join_path(root, 'config/config.v'), 'module config\npub const enabled = false\n')!
		for nested in [false, true] {
			body := '\$if cfg.enabled {\n import broken\n}\n'
			guarded := if nested { '\$if string is string {\n${body}}\n' } else { body }
			os.write_file(path, "import config as cfg\n${guarded}fn main() { println('ok') }\n")!
			for broken in ['module broken\nfn unused() { missing() }\n', 'module broken\nfn unused( { }\n'] {
				os.write_file(os.join_path(root, 'broken/broken.v'), broken)!
				result := os.exec(args)
				assert result.exit_code == 0, '${mode}, nested=${nested}: ${result.output}'
				run := os.exec([os.join_path(root, 'selected')])
				assert run.exit_code == 0 && run.output.trim_space() == 'ok', run.output
			}
		}
		os.write_file(os.join_path(root, 'config/config.v'), 'module config\npub const enabled = true\n')!
		for nested in [false, true] {
			body := '\$if cfg.enabled {\n import route_data as rd\n}\n\$if rd.yes {\n fn chosen() int { return 1 }\n} \$else {\n fn chosen() Unavailable { return missing() }\n}\n'
			guarded := if nested { '\$if string is string {\n${body}}\n' } else { body }
			os.write_file(path, 'import config as cfg\n${guarded}fn main() { assert chosen() == 1 }\n')!
			result := os.exec(args)
			assert result.exit_code == 0, '${mode}, nested=${nested}: ${result.output}'
			run := os.exec([os.join_path(root, 'selected')])
			assert run.exit_code == 0, run.output
		}
		os.write_file(path, 'import config as cfg\n\$if string is string {\n \$if cfg.enabled {\n  import broken\n }\n}\nfn main() {}\n')!
		for broken in ['module broken\nfn unused() { missing() }\n', 'module broken\nfn unused( { }\n'] {
			os.write_file(os.join_path(root, 'broken/broken.v'), broken)!
			result := os.exec(args)
			assert result.exit_code != 0, '${mode}: ${result.output}'
			assert result.output.contains('broken.v'), result.output
		}
		os.write_file(os.join_path(root, 'config/config.v'), 'module config\npub const enabled = false\n')!
		os.write_file(os.join_path(root, 'route_data/route_data.v'), data_source.replace('module route_data\n', 'module route_data\nimport config as cfg\n\$if cfg.enabled {\n import broken\n}\n'))!
		os.write_file(path, "\$if selected.starts_with('y') {\n import route_data\n}\n\$if int is int { const selected = 'yes' }\nfn main() { assert route_data.value == 3 }\n")!
		result := os.exec(args)
		assert result.exit_code == 0, '${mode}: ${result.output}'
		run := os.exec([os.join_path(root, 'selected')])
		assert run.exit_code == 0, run.output
	}
}

fn test_comptime_string_loop_bindings_restore_outer_values_after_unrolling() {
	root := os.join_path(os.vtmp_dir(), 'comptime_string_loop_shadow_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'main.v')
	os.write_file(path, "fn main() {\n route := 'constant'\n \$for route in 'runtime'.fields() {\n  \$if route.starts_with('con') { panic('wrong') } \$else { assert route == 'runtime' }\n }\n \$if route.starts_with('con') { assert true } \$else { assert false }\n}\n")!
	result := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-nocache', '-gc', 'none',
		'-o', os.join_path(root, 'restored'), path])
	assert result.exit_code == 0, result.output
	run := os.exec([os.join_path(root, 'restored')])
	assert run.exit_code == 0, run.output
}
