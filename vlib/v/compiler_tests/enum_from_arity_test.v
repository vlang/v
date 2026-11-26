import os
import v.tempname

fn enum_from_arity_compile_bad(name string, source string) string {
	dir := os.join_path(os.vtmp_dir(), 'v3_enum_from_arity_${tempname.unique_token()}')
	os.mkdir(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	bin := os.join_path(dir, 'v3_${name}')
	src := bin + '.v'
	os.write_file(src, source) or { panic(err) }
	compile := os.exec([@VEXE, src, '-b', 'c', '-o', bin])
	assert compile.exit_code != 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	return compile.output
}

fn enum_from_arity_run_bad(name string, source string, expected string) {
	output := enum_from_arity_compile_bad(name, source)
	assert output.contains(expected), output
}

fn enum_from_arity_run_good(name string, source string) string {
	dir := os.join_path(os.vtmp_dir(), 'v3_enum_from_arity_${tempname.unique_token()}')
	os.mkdir(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	bin := os.join_path(dir, 'v3_${name}')
	src := bin + '.v'
	os.write_file(src, source) or { panic(err) }
	compile := os.exec([@VEXE, src, '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	return run.output.trim_space()
}

struct EnumFromRuntimeCase {
	name     string
	source   string
	expected string
}

fn enum_from_arity_run_good_batch(cases []EnumFromRuntimeCase) {
	dir := os.join_path(os.vtmp_dir(), 'v3_enum_from_arity_${tempname.unique_token()}')
	os.mkdir(dir) or { panic(err) }
	defer { os.rmdir_all(dir) or {} }
	mut imports := []string{cap: cases.len}
	mut calls := []string{cap: cases.len}
	for i, case in cases {
		module_name := 'fixture_${i}'
		module_dir := os.join_path(dir, module_name)
		os.mkdir(module_dir) or { panic(err) }
		imports << 'import ${module_name}'
		calls << "\t\t'${case.name}' { ${module_name}.exercise() }"
		source := 'module ${module_name}\n\n' + case.source
		os.write_file(os.join_path(module_dir, '${case.name}.v'), source.replace_once('\nfn main()',
			'\npub fn exercise()')) or { panic(err) }
	}
	os.write_file(os.join_path(dir, 'main.v'), "module main\n\nimport os\n${imports.join('\n')}\n\nfn main() {\n\tmatch os.args[1] {\n${calls.join('\n')}\n\t\telse { panic('unknown fixture') }\n\t}\n}\n") or {
		panic(err)
	}
	bin := os.join_path(dir, 'program')
	compile := os.exec([@VEXE, dir, '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	for case in cases {
		run := os.exec([bin, case.name])
		assert run.exit_code == 0, '${case.name}: ${run.output}'
		assert run.output.trim_space() == case.expected, '${case.name}: expected `${case.expected}` in\n${run.output}'
	}
}

fn enum_from_arity_check_errors(name string, source string, missing_arg string, extra_arg string) {
	output := enum_from_arity_compile_bad(name, source)
	lines := source.split_into_lines()
	errors := output.split_into_lines().filter(it.contains(': error: '))
	for count, statement in {
		0: missing_arg
		2: extra_arg
	} {
		line := lines.index(statement) + 1
		assert line > 0, statement
		message := ': error: expected 1 argument, but got ${count}'
		assert errors.any(it.contains('.v:${line}:') && it.ends_with(message)), output
	}
}

fn test_enum_from_rejects_wrong_arity() {
	direct := 'enum Color {
	red
}

fn main() {
	_ := Color.from()
	_ := Color.from(0, 1)
}
'
	enum_from_arity_check_errors('bad_enum_from_direct_arity', direct, '\t_ := Color.from()',
		'\t_ := Color.from(0, 1)')

	// Check generic instantiations in a program whose direct calls pass checking.
	generic := 'enum Color {
	red
}

fn parse_missing[T]() {
	_ := T.from()
}

fn parse_extra[T](s string) {
	_ := T.from(s, 1)
}

fn main() {
	parse_missing[Color]()
	parse_extra[Color]("red")
}
'
	enum_from_arity_check_errors('bad_enum_from_generic_arity', generic, '\t_ := T.from()',
		'\t_ := T.from(s, 1)')
}

fn test_enum_from_validates_input_type_and_numeric_error() {
	enum_from_arity_run_bad('bad_enum_from_struct', 'struct Foo {}

enum Color {
	red
}

fn main() {
	_ := Color.from(Foo{})
}
',
		'cannot use `Foo` as argument 1 to `Color.from`; expected string or integer')
	enum_from_arity_run_bad('bad_generic_enum_from_struct', 'struct Foo {}

enum Color {
	red
}

fn parse[T]() {
	_ := T.from(Foo{})
}

fn main() {
	parse[Color]()
}
',
		'cannot use `Foo` as argument 1 to `T.from`; expected string or integer')

	// These diagnostics are reported when the generic functions are specialized.
	specialized := 'struct Foo {}

enum Color {
	red
	blue
}

fn parse_concrete[T](x T) !Color {
	return Color.from(x)
}

fn parse_non_enum[T](value int) ?T {
	return T.from(value)
}

// Check Result to Option returns on the concrete instance.
fn parse_enum_option[T](x int) ?T {
	return T.from(x)
}

fn checked(x int) !int {
	if x < 0 {
		return error("negative")
	}
	return x
}

fn forward[T](x int) ?T {
	return checked(x)
}

fn main() {
	_ := parse_concrete[Foo](Foo{}) or { Color.red }
	_ := parse_non_enum[Foo](0)
	println(parse_enum_option[Color](1) or { Color.red })
	println(forward[int](1) or { 0 })
}
'
	output := enum_from_arity_compile_bad('bad_enum_from_specializations', specialized)
	errors := output.split_into_lines()
	for message in [
		'cannot use `Foo` as argument 1 to `Color.from`; expected string or integer',
		'unknown function `Foo.from`',
		'cannot use `!Color` as type `?Color` in return argument',
		'cannot use `!int` as type `?int` in return argument',
	] {
		assert errors.contains(message), 'expected ${message}\n${output}'
	}
	enum_from_arity_run_good_batch([
		EnumFromRuntimeCase{
			name:     'numeric_enum_from_ierror'
			source:   'enum Color {
	red
	blue
}

fn main() {
	valid := Color.from(0) or { panic(err) }
	println(valid)
	Color.from(7) or {
		println(err.msg())
		return
	}
}
'
			expected: 'red\ninvalid value'
		},
		EnumFromRuntimeCase{
			name:     'enum_alias_numeric_from'
			source:   'type Hue = Color\n\nenum Color {\n\tred\n\tblue\n}\n\nfn main() {\n\tvalue := Hue.from(1) or { panic(err) }\n\tprintln(value)\n}\n'
			expected: 'blue'
		},
		EnumFromRuntimeCase{
			name:     'enum_alias_string_from'
			source:   'type Hue = Color\n\nenum Color {\n\tred\n\tblue\n}\n\nfn main() {\n\tvalue := Hue.from("blue") or { panic(err) }\n\tprintln(value)\n}\n'
			expected: 'blue'
		},
		EnumFromRuntimeCase{
			name:     'string_alias_enum_from'
			source:   'type Name = string

enum Color {
	red
	blue
}

fn main() {
	value := Color.from(Name("blue")) or { panic(err) }
	println(value)
}
'
			expected: 'blue'
		},
		EnumFromRuntimeCase{
			name:     'generic_string_enum_from'
			source:   'fn parse[T](s string) !T {
	return T.from(s)
}

enum Color {
	red
	blue
}

fn main() {
	value := parse[Color]("blue") or { panic(err) }
	println(value)
}
'
			expected: 'blue'
		},
		EnumFromRuntimeCase{
			name:     'generic_enum_from_valid_inputs'
			source:   'enum Color {
	red
	blue
}

fn parse[T](x T) !Color {
	return Color.from(x)
}

fn main() {
	println(parse[string]("blue") or { panic(err) })
	println(parse[int](0) or { panic(err) })
}
'
			expected: 'blue\nred'
		},
		EnumFromRuntimeCase{
			name:     'generic_enum_numeric_from'
			source:   'enum Color {
	red
	blue
}

fn parse[T](x int) !T {
	return T.from(x)
}

fn main() {
	println(parse[Color](1) or { panic(err) })
	parse[Color](7) or {
		println(err.msg())
		return
	}
}
'
			expected: 'blue\ninvalid value'
		},
	])
}

fn test_user_enum_from_and_zero_methods_are_preserved() {
	out := enum_from_arity_run_good('user_enum_from_and_zero_methods', '@[flag]
enum Mode {
	read
	write
}

@[flag]
enum BuiltinMode {
	enabled
}

enum Color {
	red
	blue
}

struct StaticSource {}

struct ValueSource {
	label string
}

fn Color.from(value int) ?Color {
	println("custom from \${value}")
	if value == 99 {
		return .blue
	}
	return none
}

fn Mode.zero() Mode {
	println("custom enum zero")
	return .read
}

fn StaticSource.zero() Mode {
	println("custom static zero")
	return .write
}

fn (source ValueSource) zero() Mode {
	println("custom instance zero \${source.label}")
	return .read
}

fn parse[T](value int) ?T {
	return T.from(value)
}

fn main() {
	println(int(Color.from(99) or { panic("none") }))
	println(int(parse[Color](99) or { panic("none") }))
	println(int(Mode.zero()))
	println(int(StaticSource.zero()))
	println(int(ValueSource{label: "value"}.zero()))
	println(int(BuiltinMode.zero()))
}
')
	assert out == 'custom from 99\n1\ncustom from 99\n1\ncustom enum zero\n1\ncustom static zero\n2\ncustom instance zero value\n1\n0'
}
