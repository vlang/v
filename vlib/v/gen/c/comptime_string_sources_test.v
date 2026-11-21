module main

import os

// exe_suffix is needed because os.exec spawns an absolute path as written, without adding .exe.
const exe_suffix = $if windows { '.exe' } $else { '' }

fn test_parallel_computed_constant_guards_select_the_same_runtime_branch() {
	root := os.join_path(os.vtmp_dir(), 'comptime_parallel_guards_codegen_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	// Keep the constants, guards and callers in separate padded files so the
	// same project exercises cross-file evaluation with both worker settings.
	sources := [
		"const direct_has_get = 'GET /users'.starts_with('GET')
const chained_has_get = 'GET /users'.starts_with('GET')
const else_if_has_get = 'GET /users'.starts_with('POST')
const match_has_get = 'GET /users'.starts_with('GET')
",
		"\$if chained_has_get { const chained_chosen = 'yes' } \$else { const chained_chosen = 'no' }
\$if else_if_has_get { const else_if_chosen = 'wrong' } \$else \$if false { const inactive = 'unused' } \$else \$if true { const else_if_chosen = 'yes' } \$else { const dead = 'unused' }
const following = 'yes'
\$match match_has_get { true { const match_chosen = 'yes' } \$else { const match_chosen = 'no' } }
\$if match_chosen == 'yes' { fn match_selected() string { return 'yes' } } \$else { fn match_selected() string { return 'no' } }
",
		"\$if direct_has_get { fn direct_selected() string { return 'yes' } } \$else { fn direct_selected() string { return 'no' } }
\$if chained_chosen == 'yes' { fn chained_selected() string { return 'yes' } } \$else { fn chained_selected() string { return 'no' } }
\$if else_if_chosen == 'yes' && following == 'yes' { fn else_if_selected() string { return 'yes' } } \$else { fn else_if_selected() string { return 'no' } }
",
		"fn main() {
	println('direct: ' + direct_selected())
	println('chained: ' + chained_selected())
	println('else_if: ' + else_if_selected())
	println('match: ' + match_selected())
}
",
	]
	for i, source in sources {
		os.write_file(os.join_path(root, '${i}.v'), source + '\n'.repeat(40000 - source.len))!
	}
	expected := ['direct: yes', 'chained: yes', 'else_if: yes', 'match: yes']
	// A Windows `clang` usually targets the MSVC ABI, which V cannot build Windows
	// programs with, so use the MinGW gcc there.
	cc := $if windows { 'gcc' } $else { 'clang' }
	for serial in [false, true] {
		binary := os.join_path(root, 'selected_${serial}${exe_suffix}')
		mut args := ['-new-compiler', '-no-retry-compilation', '-nocache', '-cc', cc, '-gc', 'none',
			'-o', binary]
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
		assert result.output.split_into_lines() == expected, 'serial=${serial}: ${result.output}'
	}
}

fn comptime_router_body_with_literal_values(code string, body string) string {
	mut literals := map[string]string{}
	for line in code.split_into_lines() {
		is_macro := line.starts_with('_SL(_str_')
		if !is_macro && !line.starts_with('static const string _str_') { continue }
		name := if is_macro {
			line.all_after('_SL(').all_before(', ')
		} else {
			line.all_after('static const string ').all_before(' = ')
		}
		assert name !in literals, name
		literals[name] = if is_macro {
			'{' + line.all_after(', ').all_before_last(');') + ', 1}'
		} else {
			line.all_after(' = ').trim_string_right(';')
		}
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
	for macro in [false, true] {
		users := if macro { '"users", 5' } else { '{"users", 5, 1}' }
		posts := if macro { '"posts", 5' } else { '{"posts", 5, 1}' }
		prefix := if macro { '_SL(' } else { 'static const string ' }
		separator := if macro { ', ' } else { ' = ' }
		suffix := if macro { ');' } else { ';' }
		first := comptime_router_body_with_literal_values('${prefix}_str_1${separator}${users}${suffix}',
			'string path = _str_1;')
		renumbered := comptime_router_body_with_literal_values('${prefix}_str_10${separator}${users}${suffix}',
			'string path = _str_10;')
		changed := comptime_router_body_with_literal_values('${prefix}_str_10${separator}${posts}${suffix}',
			'string path = _str_10;')
		assert first == renumbered
		assert first != changed
	}
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

fn check_comptime_string_diagnostics(root string, name string, source string, expected []string) ! {
	path := os.join_path(root, '${name}.v')
	os.write_file(path, source)!
	result := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-nocache', '-nocolor',
		'-o', os.join_path(root, '${name}.c'), path])
	assert result.exit_code != 0, result.output
	path_prefix := os.real_path(path).replace('\\', '/')
	errors := result.output.replace('\\', '/').split_into_lines().filter(it.starts_with(path_prefix + ':')
		&& it.contains(': error: '))
	assert errors.len == expected.len, result.output
	// Each invalid function is on a separate line so its own error must be present.
	for index, message in expected {
		location := '${path_prefix}:${index + 1}:'
		assert errors.any(it.starts_with(location) && it.contains(message)), result.output
	}
}

fn test_comptime_string_sources_reject_runtime_or_unsupported_operands() {
	root := os.join_path(os.vtmp_dir(), 'comptime_string_sources_invalid_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	loop_error := 'on a compile-time-known string'
	condition_error := 'cannot evaluate `\$if` condition'
	// Checker errors stop the build before the transformer can report its errors.
	check_comptime_string_diagnostics(root, 'checker', r"fn mutable_source() { mut path := 'a/b'; path = 'c/d'; $for part in path.split('/') { println(part) } }
fn runtime_source(path string) { $for part in path.split('/') { println(part) } }
fn unsupported_source() { $for part in 'abc'.bytes() { println(part) } }
fn mutable_condition() { mut path := 'a'; path = 'b'; $if path.starts_with('a') { println('bad') } }
fn overflow_high_slice() { $for part in 'abc'[..4294967296].fields() { println(part) } }
fn main() { mutable_source(); runtime_source('a/b'); unsupported_source(); mutable_condition(); overflow_high_slice() }
", [loop_error, loop_error, loop_error, '`path` is mut and may have changed since its definition',
		loop_error])!
	check_comptime_string_diagnostics(root, 'transform', r"fn overflow_slice() { $for part in 'abc'[4294967296..].fields() { println(part) } }
fn overflow_condition() { $if 'abc'[4294967296..].starts_with('a') { println('bad') } }
fn unsupported_condition() { $if 'abc'.repeat(1) == 'abc' { println('bad') } }
fn main() { overflow_slice(); overflow_condition(); unsupported_condition() }
", [loop_error, condition_error, condition_error])!
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
		'-o', os.join_path(root, 'scope${exe_suffix}'), path])
	assert result.exit_code == 0, result.output
	run := os.exec([os.join_path(root, 'scope${exe_suffix}')])
	assert run.exit_code == 0, run.output
	alias_source := os.read_file(path)!.replace('import route_data', 'import route_data as rd').replace('route_data.', 'rd.')
	os.write_file(path, alias_source)!
	alias_result := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-nocache', '-gc',
		'none', '-o', os.join_path(root, 'alias_scope${exe_suffix}'), path])
	assert alias_result.exit_code == 0, alias_result.output
	alias_run := os.exec([os.join_path(root, 'alias_scope${exe_suffix}')])
	assert alias_run.exit_code == 0, alias_run.output
}

fn test_static_string_conditions_discard_unavailable_top_level_declarations() {
	root := os.join_path(os.vtmp_dir(), 'comptime_string_top_level_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'main.v')
	os.write_file(path, "const route = 'GET /users'.all_after(' ').trim_left('/')\n\$if route.starts_with('users') {\n fn selected() int { return 1 }\n} \$else {\n fn selected() Unavailable { return unavailable() }\n}\n\$if 'ABC'.to_lower().starts_with('z') {\n fn fallback() Unavailable { return unavailable() }\n} \$else {\n fn fallback() int { return 2 }\n}\nfn main() { assert selected() == 1; assert fallback() == 2 }\n")!
	result := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-nocache', '-gc', 'none',
		'-o', os.join_path(root, 'selected${exe_suffix}'), path])
	assert result.exit_code == 0, result.output
	run := os.exec([os.join_path(root, 'selected${exe_suffix}')])
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
		'-o', os.join_path(root, 'guard_order${exe_suffix}'), path])
	assert result.exit_code == 0, result.output
	run := os.exec([os.join_path(root, 'guard_order${exe_suffix}')])
	assert run.exit_code == 0, run.output
}

fn test_immutable_local_string_sources_in_normal_function_blocks() {
	root := os.join_path(os.vtmp_dir(), 'comptime_string_local_source_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'main.v')
	os.write_file(path, "fn main() {\n path := 'GET /users/:id/posts'.all_after(' ').trim_left('/')\n \$for segment in path.split('/') {\n \$if !segment.starts_with(':') { println(segment) }\n }\n}\n")!
	result := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-nocache', '-gc', 'none',
		'-o', os.join_path(root, 'local${exe_suffix}'), path])
	assert result.exit_code == 0, result.output
	run := os.exec([os.join_path(root, 'local${exe_suffix}')])
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
		'-o', os.join_path(root, 'restored${exe_suffix}'), path])
	assert result.exit_code == 0, result.output
	run := os.exec([os.join_path(root, 'restored${exe_suffix}')])
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
	broken_path := os.join_path(root, 'broken/broken.v')
	invalid_function := 'fn unused() { missing() }\n'
	invalid_syntax := 'fn unused( { }\n'
	// Distinct imports let the direct and nested guards exercise separate modules.
	for name, body in {
		'broken_syntax':        invalid_syntax
		'nested_broken':        invalid_function
		'nested_broken_syntax': invalid_syntax
		'nested_data':          'pub const yes = true\npub const value = 2\n'
	} {
		module_dir := os.join_path(root, name)
		os.mkdir_all(module_dir)!
		os.write_file(os.join_path(module_dir, '${name}.v'), 'module ${name}\n${body}')!
	}
	path := os.join_path(root, 'main.v')
	executable := os.join_path(root, 'selected${exe_suffix}')
	saved_no_file_index := os.getenv('V3_NO_FILE_IDX')
	defer { os.setenv('V3_NO_FILE_IDX', saved_no_file_index, true) }
	for mode in ['parallel', 'serial', 'full_scan'] {
		os.setenv('V3_NO_FILE_IDX', if mode == 'full_scan' { '1' } else { '' }, true)
		mut args := [@VEXE, '-new-compiler', '-no-retry-compilation', '-nocache', '-gc', 'none']
		if mode == 'serial' { args << '-no-parallel' }
		args << ['-o', executable, path]
		os.write_file(os.join_path(root, 'route_data/route_data.v'), data_source)!
		os.write_file(os.join_path(root, 'config/config.v'), 'module config\npub const enabled = false\n')!
		os.write_file(broken_path, 'module broken\n' + invalid_function)!
		os.write_file(path, r"import config as cfg
$if cfg.enabled {
 import broken
 import broken_syntax
}
$if string is string {
 $if cfg.enabled {
  import nested_broken
  import nested_broken_syntax
 }
}
fn main() { println('ok') }
")!
		disabled := os.exec(args)
		assert disabled.exit_code == 0, '${mode}: ${disabled.output}'
		disabled_run := os.exec([executable])
		assert disabled_run.exit_code == 0 && disabled_run.output.trim_space() == 'ok', disabled_run.output

		os.write_file(os.join_path(root, 'config/config.v'), 'module config\npub const enabled = true\n')!
		os.write_file(path, r'import config as cfg
$if cfg.enabled {
 import route_data as rd
}
$if string is string {
 $if cfg.enabled {
  import nested_data as nd
 }
 $if nd.yes {
  fn chosen_nested() int { return nd.value }
 } $else {
  fn chosen_nested() Unavailable { return missing() }
 }
}
$if rd.yes {
 fn chosen() int { return 1 }
} $else {
 fn chosen() Unavailable { return missing() }
}
fn main() {
 assert chosen() == 1
 assert chosen_nested() == 2
}
')!
		enabled := os.exec(args)
		assert enabled.exit_code == 0, '${mode}: ${enabled.output}'
		enabled_run := os.exec([executable])
		assert enabled_run.exit_code == 0, enabled_run.output

		os.write_file(path, 'import config as cfg\n\$if string is string {\n \$if cfg.enabled {\n  import broken\n }\n}\nfn main() {}\n')!
		for body in [invalid_function, invalid_syntax] {
			os.write_file(broken_path, 'module broken\n' + body)!
			result := os.exec(args)
			assert result.exit_code != 0, '${mode}: ${result.output}'
			assert result.output.contains('broken.v'), result.output
		}
		os.write_file(os.join_path(root, 'config/config.v'), 'module config\npub const enabled = false\n')!
		os.write_file(os.join_path(root, 'route_data/route_data.v'), data_source.replace('module route_data\n', 'module route_data\nimport config as cfg\n\$if cfg.enabled {\n import broken\n}\n'))!
		os.write_file(path, "\$if selected.starts_with('y') {\n import route_data\n}\n\$if int is int { const selected = 'yes' }\nfn main() { assert route_data.value == 3 }\n")!
		result := os.exec(args)
		assert result.exit_code == 0, '${mode}: ${result.output}'
		run := os.exec([executable])
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
		'-o', os.join_path(root, 'restored${exe_suffix}'), path])
	assert result.exit_code == 0, result.output
	run := os.exec([os.join_path(root, 'restored${exe_suffix}')])
	assert run.exit_code == 0, run.output
}
