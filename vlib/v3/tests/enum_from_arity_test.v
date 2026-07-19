import os

const enum_from_arity_vexe = @VEXE
const enum_from_arity_tests_dir = os.dir(@FILE)
const enum_from_arity_v3_dir = os.dir(enum_from_arity_tests_dir)
const enum_from_arity_vlib_dir = os.dir(enum_from_arity_v3_dir)
const enum_from_arity_v3_src = os.join_path(enum_from_arity_v3_dir, 'v3.v')

fn enum_from_arity_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_enum_from_arity_test_${os.getpid()}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${enum_from_arity_vexe} -gc none -path "${enum_from_arity_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${enum_from_arity_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn enum_from_arity_run_bad(v3_bin string, name string, source string, expected string) {
	src := os.join_path(os.temp_dir(), 'v3_${name}_${os.getpid()}.v')
	os.write_file(src, source) or { panic(err) }
	bin := os.join_path(os.temp_dir(), 'v3_${name}_${os.getpid()}')
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains(expected), compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
}

fn enum_from_arity_run_good(v3_bin string, name string, source string) string {
	src := os.join_path(os.temp_dir(), 'v3_${name}_${os.getpid()}.v')
	os.write_file(src, source) or { panic(err) }
	bin := os.join_path(os.temp_dir(), 'v3_${name}_${os.getpid()}')
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	return run.output.trim_space()
}

fn test_enum_from_rejects_wrong_arity() {
	v3_bin := enum_from_arity_build_v3()
	enum_from_arity_run_bad(v3_bin, 'bad_enum_from_missing_arg', 'enum Color {
	red
}

fn main() {
	_ := Color.from()
}
',
		'argument count mismatch for `Color.from`: expected 1, got 0')
	enum_from_arity_run_bad(v3_bin, 'bad_enum_from_extra_arg', 'enum Color {
	red
}

fn main() {
	_ := Color.from(0, 1)
}
',
		'argument count mismatch for `Color.from`: expected 1, got 2')
	enum_from_arity_run_bad(v3_bin, 'bad_generic_enum_from_missing_arg', 'enum Color {
	red
}

fn parse[T]() {
	_ := T.from()
}

fn main() {
	parse[Color]()
}
',
		'argument count mismatch for `T.from`: expected 1, got 0')
	enum_from_arity_run_bad(v3_bin, 'bad_generic_enum_from_extra_arg', 'enum Color {
	red
}

fn parse[T](s string) {
	_ := T.from(s, 1)
}

fn main() {
	parse[Color]("red")
}
',
		'argument count mismatch for `T.from`: expected 1, got 2')
}

fn test_enum_from_validates_input_type_and_numeric_error() {
	v3_bin := enum_from_arity_build_v3()
	enum_from_arity_run_bad(v3_bin, 'bad_enum_from_struct', 'struct Foo {}

enum Color {
	red
}

fn main() {
	_ := Color.from(Foo{})
}
',
		'cannot use `Foo` as argument 1 to `Color.from`; expected string or integer')
	enum_from_arity_run_bad(v3_bin, 'bad_generic_enum_from_struct', 'struct Foo {}

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
	enum_from_arity_run_bad(v3_bin, 'bad_specialized_enum_from_generic_arg', 'struct Foo {}

enum Color {
	red
}

fn parse[T](x T) ?Color {
	return Color.from(x)
}

fn main() {
	_ := parse[Foo](Foo{})
}
',
		'cannot use `Foo` as argument 1 to `Color.from`; expected string or integer')
	out := enum_from_arity_run_good(v3_bin, 'numeric_enum_from_ierror', 'enum Color {
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
	enum_alias_out := enum_from_arity_run_good(v3_bin, 'enum_alias_numeric_from',
		'type Hue = Color\n\nenum Color {\n\tred\n\tblue\n}\n\nfn main() {\n\tvalue := Hue.from(1) or { panic(err) }\n\tprintln(value)\n}\n')
	assert enum_alias_out == 'blue'
	enum_alias_string_out := enum_from_arity_run_good(v3_bin, 'enum_alias_string_from',
		'type Hue = Color\n\nenum Color {\n\tred\n\tblue\n}\n\nfn main() {\n\tvalue := Hue.from("blue") or { panic(err) }\n\tprintln(value)\n}\n')
	assert enum_alias_string_out == 'blue'
	alias_out := enum_from_arity_run_good(v3_bin, 'string_alias_enum_from', 'type Name = string

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
	generic_out := enum_from_arity_run_good(v3_bin, 'generic_string_enum_from', 'fn parse[T](s string) ?T {
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
	specialized_inputs_out := enum_from_arity_run_good(v3_bin, 'generic_enum_from_valid_inputs', 'enum Color {
	red
	blue
}

fn parse[T](x T) ?Color {
	return Color.from(x)
}

fn main() {
	println(parse[string]("blue") or { panic(err) })
	println(parse[int](0) or { panic(err) })
}
')
	assert specialized_inputs_out == 'blue\nred'
	generic_enum_numeric_out := enum_from_arity_run_good(v3_bin, 'generic_enum_numeric_from', 'enum Color {
	red
	blue
}

fn parse[T](x int) ?T {
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
