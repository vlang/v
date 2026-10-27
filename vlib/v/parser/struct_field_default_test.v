module parser

import os
import v.pref

fn test_struct_field_default_boundaries_stay_outside_nested_expressions() {
	fields := [
		'value bool = (1\n in [1])',
		'value bool = identity(1\n in [1])',
		'value bool = !(1\n in [1])',
		'value []bool = [1\n in [1]]',
		"value map[string]bool = {'key': 1\n in [1]}",
		'value Nested = Nested{value: 1\n in [1]}',
		'value bool = if 1\n in [1] { true } else { false }',
		'value int = [3][if 1\n in [1] { 0 } else { 1 }]',
		'value fn () int = fn () int { value := optional_value()\n or { return 0 }\n return value }',
		'value fn () int = fn () int { return if 1\n in [1] { 3 } else { 0 } }',
		'value bool = (optional_bool()\n or { false })',
		// Recursive operands still stop before the next keyword-named field.
		'value int = -1',
		'value int = 1 + 2 * 3',
		'value bool = 1 in [1]',
		'value bool = 1 !in [2]',
	]
	for field in fields {
		for aggregate in ['struct Defaults {\n%s\n}', 'union Defaults {\n%s\n}',
			'struct Container { nested struct {\n%s\n} }'] {
			source := aggregate.replace('%s', '${field}\n in int\n is int\n as int\n or int\n')
			for is_fmt in [false, true] {
				mut prefs := pref.new_preferences()
				prefs.is_fmt = is_fmt
				mut p := Parser.new(prefs)
				a := p.parse_text('struct_field_default.v', source)
				assert p.diagnostics.len == 0, '${source}: ${p.diagnostics}'
				for name in ['in', 'is', 'as', 'or'] {
					assert a.nodes.filter(it.kind == .field_decl && it.value == name).len == 1, '${source}: missing field ${name}'
				}
			}
		}
	}
}

fn test_struct_field_default_error_does_not_change_later_expression_context() {
	for invalid in ['struct Bad { value bool = (1 + ,) }', 'struct Bad { value bool = identity(1 + ,) }',
		'struct Bad { callback fn () int = fn () int { return , } }',
		'struct Bad { value bool = (1 in [1]'] {
		for is_fmt in [false, true] {
			mut prefs := pref.new_preferences()
			prefs.is_fmt = is_fmt
			mut p := Parser.new(prefs)
			p.parse_text('invalid_default.v', invalid)
			assert p.diagnostics.len > 0, invalid
			previous_errors := p.diagnostics.len
			a := p.parse_text('after_invalid_default.v', 'fn after() bool {\n value := optional_bool()\n or { return false }\n return value && (1\n in [1])\n}')
			assert p.diagnostics.len == previous_errors, p.diagnostics.str()
			assert a.nodes.filter(it.kind == .or_expr).len == 1
			assert a.nodes.filter(it.kind == .in_expr).len >= 1
		}
	}
}

fn test_match_struct_field_default_preserves_or_field() {
	for value in ['match true { true { 1 } else { 2 } }', '1 + match true { true { 1 } else { 2 } }',
		'-match true { true { 1 } else { 2 } }'] {
		for separator in ['\n', ';'] {
			for aggregate in ['struct Defaults {\n%s\n}', 'union Defaults {\n%s\n}',
				'struct Container { nested struct {\n%s\n} }'] {
				source := aggregate.replace('%s', 'value int = ${value}${separator}or int')
				for is_fmt in [false, true] {
					mut prefs := pref.new_preferences()
					prefs.is_fmt = is_fmt
					mut p := Parser.new(prefs)
					a := p.parse_text('match_default.v', source)
					assert p.diagnostics.len == 0, '${source}: ${p.diagnostics}'
					assert a.nodes.filter(it.kind == .field_decl && it.value == 'or').len == 1, source
					assert a.nodes.filter(it.kind == .or_expr).len == 0, source
				}
			}
		}
	}
}

fn test_match_struct_field_default_keeps_nested_or_handlers() {
	for source in [
		'struct Defaults { value int = (match true { true { optional_value() } else { optional_value() } }\n or { 0 })\n or int }',
		'struct Defaults { value int = identity(match true { true { optional_value() } else { optional_value() } }\n or { 0 })\n or int }',
		'struct Defaults { value int = match true { true { optional_value() } else { optional_value() } } or { 0 }\n or int }',
		'struct Defaults { value int = match true { true { match true { true { optional_value() } else { optional_value() } }\n or { 0 } } else { 0 } }\n or int }',
		'struct Defaults { callback fn () int = fn () int { return match true { true { optional_value() } else { optional_value() } }\n or { 0 } }\n or int }',
		'fn control() int { value := match true { true { optional_value() } else { optional_value() } }\n or { 0 }\n return value }',
		'fn control() { match true { true { optional_value() } else { optional_value() } }\n or { 0 } }',
	] {
		for is_fmt in [false, true] {
			mut prefs := pref.new_preferences()
			prefs.is_fmt = is_fmt
			mut p := Parser.new(prefs)
			a := p.parse_text('match_or_handler.v', source)
			assert p.diagnostics.len == 0, '${source}: ${p.diagnostics}'
			assert a.nodes.filter(it.kind == .or_expr).len == 1, source
		}
	}
}

fn test_nested_struct_field_defaults_compile_and_run() {
	path := os.join_path(os.vtmp_dir(), 'nested_struct_defaults_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	os.write_file(path, 'struct Defaults {\n grouped bool = (1\n in [1])\n argument bool = identity(1\n in [1])\n callback fn () int = fn () int {\n value := optional_value()\n or { return 0 }\n return value\n }\n in int\n matched int = match true { true { 3 } else { 0 } }\n or int\n}\nfn identity(value bool) bool { return value }\nfn optional_value() ?int { return 42 }\nfn main() {\n value := Defaults{in: 7, or: 8}\n assert value.grouped\n assert value.argument\n assert value.callback() == 42\n assert value.matched == 3\n assert value.in == 7\n assert value.or == 8\n}\n')!
	mut child := os.new_process(@VEXE)
	child.set_args(['-new-compiler', '-no-retry-compilation', '-cc', 'clang', '-gc', 'none', 'run',
		path])
	mut environment := os.environ()
	environment.delete('VFLAGS')
	environment.delete('VOSARGS')
	environment['V_MACOS_V3_NO_FALLBACK'] = '1'
	child.set_environment(environment)
	child.set_redirect_stdio_merged()
	child.run()
	defer { child.close() }
	output := child.stdout_slurp()
	child.wait()
	assert child.code == 0, output
}
