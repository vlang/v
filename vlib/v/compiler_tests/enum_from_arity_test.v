import os
import v.tempname

const enum_from_arity_vexe = @VEXE
const enum_from_arity_tests_dir = os.dir(@FILE)
const enum_from_arity_v3_dir = os.dir(enum_from_arity_tests_dir)
const enum_from_arity_vlib_dir = os.dir(enum_from_arity_v3_dir)
const enum_from_arity_v3_src = os.join_path(enum_from_arity_v3_dir, 'v.v')

const enum_from_arity_fixture_dir = os.join_path(os.vtmp_dir(), 'v3_enum_from_arity_fixtures_${tempname.unique_token()}')
const enum_from_arity_compiler = os.join_path(enum_from_arity_fixture_dir, 'v3') + $if windows { '.exe' } $else { '' }

fn testsuite_begin() {
	os.mkdir_all(enum_from_arity_fixture_dir) or { panic(err) }
	build :=
		os.exec([enum_from_arity_vexe, '-gc', 'none', '-path',
			'${enum_from_arity_vlib_dir}' + '|@vlib|@vmodules', '-o', enum_from_arity_compiler,
			'${enum_from_arity_v3_src}'])
	assert build.exit_code == 0, build.output
}

fn testsuite_end() {
	os.rmdir_all(enum_from_arity_fixture_dir) or {}
}

fn enum_from_arity_run_bad(name string, source string, expected string) {
	src := os.join_path(enum_from_arity_fixture_dir, 'v3_${name}.v')
	os.write_file(src, source) or { panic(err) }
	bin := os.join_path(enum_from_arity_fixture_dir, 'v3_${name}')
	compile := os.exec([enum_from_arity_compiler, '${src}', '-b', 'c', '-o', bin])
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains(expected), compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
}

fn enum_from_arity_run_good(name string, source string) string {
	src := os.join_path(enum_from_arity_fixture_dir, 'v3_${name}.v')
	os.write_file(src, source) or { panic(err) }
	bin := os.join_path(enum_from_arity_fixture_dir, 'v3_${name}')
	compile := os.exec([enum_from_arity_compiler, '${src}', '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	return run.output.trim_space()
}

fn test_enum_from_rejects_wrong_arity() {
	enum_from_arity_run_bad('bad_enum_from_missing_arg', 'enum Color {
	red
}

fn main() {
	_ := Color.from()
}
',
		'expected 1 argument, but got 0')
	enum_from_arity_run_bad('bad_enum_from_extra_arg', 'enum Color {
	red
}

fn main() {
	_ := Color.from(0, 1)
}
',
		'expected 1 argument, but got 2')
	enum_from_arity_run_bad('bad_generic_enum_from_missing_arg', 'enum Color {
	red
}

fn parse[T]() {
	_ := T.from()
}

fn main() {
	parse[Color]()
}
',
		'expected 1 argument, but got 0')
	enum_from_arity_run_bad('bad_generic_enum_from_extra_arg', 'enum Color {
	red
}

fn parse[T](s string) {
	_ := T.from(s, 1)
}

fn main() {
	parse[Color]("red")
}
',
		'expected 1 argument, but got 2')
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
	enum_from_arity_run_bad('bad_specialized_enum_from_generic_arg', 'struct Foo {}

enum Color {
	red
}

fn parse[T](x T) !Color {
	return Color.from(x)
}

fn main() {
	_ := parse[Foo](Foo{}) or { Color.red }
}
',
		'cannot use `Foo` as argument 1 to `Color.from`; expected string or integer')
	enum_from_arity_run_bad('bad_generic_enum_from_non_enum_receiver', 'struct Foo {}

fn parse[T](value int) ?T {
	return T.from(value)
}

fn main() {
	_ := parse[Foo](0)
}
',
		'unknown function `Foo.from`')
	// A specialization that returns a Result from an Option function is rejected like the
	// same return in an ordinary function, instead of failing in the C compiler.
	enum_from_arity_run_bad('bad_generic_enum_from_option_return', 'enum Color {
	red
	blue
}

fn parse[T](x int) ?T {
	return T.from(x)
}

fn main() {
	println(parse[Color](1) or { Color.red })
}
',
		'cannot use `!Color` as type `?Color` in return argument')
	enum_from_arity_run_bad('bad_generic_result_call_option_return', 'fn checked(x int) !int {
	if x < 0 {
		return error("negative")
	}
	return x
}

fn forward[T](x int) ?T {
	return checked(x)
}

fn main() {
	println(forward[int](1) or { 0 })
}
',
		'cannot use `!int` as type `?int` in return argument')
	out := enum_from_arity_run_good('numeric_enum_from_ierror', 'enum Color {
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
')
	assert out == 'red\ninvalid value'
	enum_alias_out := enum_from_arity_run_good('enum_alias_numeric_from',
		'type Hue = Color\n\nenum Color {\n\tred\n\tblue\n}\n\nfn main() {\n\tvalue := Hue.from(1) or { panic(err) }\n\tprintln(value)\n}\n')
	assert enum_alias_out == 'blue'
	enum_alias_string_out := enum_from_arity_run_good('enum_alias_string_from',
		'type Hue = Color\n\nenum Color {\n\tred\n\tblue\n}\n\nfn main() {\n\tvalue := Hue.from("blue") or { panic(err) }\n\tprintln(value)\n}\n')
	assert enum_alias_string_out == 'blue'
	alias_out := enum_from_arity_run_good('string_alias_enum_from', 'type Name = string

enum Color {
	red
	blue
}

fn main() {
	value := Color.from(Name("blue")) or { panic(err) }
	println(value)
}
')
	assert alias_out == 'blue'
	generic_out := enum_from_arity_run_good('generic_string_enum_from', 'fn parse[T](s string) !T {
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
')
	assert generic_out == 'blue'
	specialized_inputs_out := enum_from_arity_run_good('generic_enum_from_valid_inputs', 'enum Color {
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
')
	assert specialized_inputs_out == 'blue\nred'
	generic_enum_numeric_out := enum_from_arity_run_good('generic_enum_numeric_from', 'enum Color {
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
')
	assert generic_enum_numeric_out == 'blue\ninvalid value'
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
