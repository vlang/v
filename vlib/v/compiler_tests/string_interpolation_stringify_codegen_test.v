import os
import v.tempname

fn compile_string_interp_input(source string, bin string) os.Result {
	src := bin + '.v'
	os.write_file(src, source) or { panic(err) }
	return os.exec([@VEXE, src, '-b', 'c', '-nocolor', '-o', bin])
}

fn test_string_interpolation_callees_defer_and_aliases() {
	dir := os.join_path(os.vtmp_dir(), 'v3_string_interp_${tempname.unique_token()}')
	os.mkdir(dir)!
	defer { os.rmdir_all(dir) or {} }
	files := {
		// Interpolation keeps the nested method call and the custom str helpers reachable.
		'callees.v':        'struct Begin {
	value int
}

fn (b Begin) to_json() string {
	return \'\${b.value}\'
}

struct Message {
	begin Begin
}

fn (m Message) to_json() string {
	return \'{"begin":\${m.begin.to_json()}}\'
}

fn check_interpolated_method_call() {
	msg := Message{
		begin: Begin{
			value: 7
		}
	}
	println(msg.to_json())
}

struct Wrap {}

fn str_helper() string {
	return \'wrapped\'
}

fn (w Wrap) str() string {
	_ = w
	return str_helper()
}

fn check_discovered_str_helper() {
	println(\'\${Wrap{}}\')
}

type Parser[T] = fn (string) T

struct ParserHolder {
	parser Parser[int] @[required]
}

fn (parser Parser[T]) str[T]() string {
	_ = parser
	return \'parser\'
}

fn new_parser() Parser[int] {
	return fn (text string) int {
		return text.int()
	}
}

fn check_generic_fn_alias_str() {
	parser := new_parser()
	assert \'\${parser}\' == \'parser\'
	holder := ParserHolder{
		parser: parser
	}
	assert \'\${holder}\'.contains(\'parser: parser\')
	println(\'ok\')
}

fn case_callees() {
	check_interpolated_method_call()
	check_discovered_str_helper()
	check_generic_fn_alias_str()
}
'
		'defer_generics.v': "struct Box[T] {
	value T
}

fn (b Box[T]) str() string {
	return 'box:' + '\${b.value}'
}

struct PlainBox[T] {
	value T
}

fn show_box[T](b Box[T]) {
	defer {
		println('box \${b}')
	}
	println('body-box')
}

fn show_plain_box[T](b PlainBox[T]) {
	list := [b]
	mut lookup := map[string]PlainBox[T]{}
	lookup['one'] = b
	mut rune_lookup := map[rune]PlainBox[T]{}
	rune_lookup[`x`] = b
	// A `map[PlainBox[T]]rune` was printed here too; struct map keys are rejected since
	// 2c7c304cc8, also in specialized generic bodies.
	defer {
		println('plain \${b} \${list} \${lookup} \${rune_lookup}')
	}
	println('body-plain')
}

fn show_value[D](value D) {
	defer {
		println('value \${value}')
	}
	println('body-d')
}

fn case_defer_generics() {
	show_box(Box[int]{
		value: 7
	})
	show_plain_box(PlainBox[int]{
		value: 8
	})
	show_value(9)
}
"
		'aliases.v':        "type MyString = string
type A = string
type B = A
type I = int
type J = I
type Truth = bool
type Toggle = Truth

fn case_aliases() {
	s := MyString('y')
	assert 'x' + s == 'xy'
	assert s + 'z' == 'yz'
	println('ok')
	println(B('x'))
	println(J(7))
	println(Toggle(true))
}
"
		'main.v':           "module main

fn main() {
	match arguments()[1] {
		'callees' { case_callees() }
		'defer_generics' { case_defer_generics() }
		'aliases' { case_aliases() }
		else { panic('unknown fixture') }
	}
}
"
	}
	for file, source in files {
		os.write_file(os.join_path(dir, file), source)!
	}
	bin := os.join_path(dir, 'v3_string_interp_success')
	compile := os.exec([@VEXE, dir, '-b', 'c', '-nocolor', '-o', bin])
	// Formatting map[string]PlainBox[T] uses prelude helpers that need f64__str.
	// Keep the typed map interpolation fixture in its own build so it cannot supply that helper.
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	for name, expected in {
		'callees':        '{"begin":7}\nwrapped\nok'
		'defer_generics': "body-box
box box:7
body-plain
plain PlainBox[int]{
    value: 8
} [PlainBox[int]{
    value: 8
}] {'one': PlainBox[int]{
    value: 8
}} {`x`: PlainBox[int]{
    value: 8
}}
body-d
value 9"
		'aliases':        'ok\nx\n7\ntrue'
	} {
		run := os.exec([bin, name])
		assert run.exit_code == 0, '${name}: ${run.output}'
		assert run.output.trim_space() == expected, '${name}: ${run.output}'
	}
}

fn test_string_plus_does_not_stringify_non_strings() {
	dir := os.join_path(os.vtmp_dir(), 'v3_string_interp_${tempname.unique_token()}')
	os.mkdir(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	src := "fn main() {
	_ := 'x' + 1
	_ := 'x' + map[string]int{}
}
"
	bin := os.join_path(dir, 'v3_string_plus_rejections')
	result := compile_string_interp_input(src, bin)
	assert result.exit_code != 0, result.output
	assert !result.output.contains('C compilation failed'), result.output
	expected := {
		"\t_ := 'x' + 1":                'infix expr: cannot use `int literal` (right expression) as `string`'
		"\t_ := 'x' + map[string]int{}": 'infix expr: cannot use `map[string]int` (right expression) as `string`'
	}
	lines := src.split_into_lines()
	diagnostics := result.output.split_into_lines().filter(it.contains(': error: '))
	for statement, message in expected {
		line := lines.index(statement) + 1
		assert line > 0, statement
		location := 'v3_string_plus_rejections.v:${line}:'
		assert diagnostics.any(it.contains(location) && it.contains(message)), 'expected ${message} at ${location}\n${result.output}'
	}
}

fn test_typed_and_joined_string_interpolation() {
	dir := os.join_path(os.vtmp_dir(), 'v3_string_interp_${tempname.unique_token()}')
	os.mkdir(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	src := 'const prefix = \'hello\'
const greeting = \'\${prefix} world!\'

struct Counter {
mut:
	n int
}

fn (mut c Counter) next() int {
	c.n++
	return c.n
}

fn (mut c Counter) values() []int {
	c.n++
	return [c.n]
}

fn (mut c Counter) maybe() ?int {
	c.n++
	return c.n
}

fn decorate(text string) string {
	return \'<\${text}> / \${text}\'
}

fn generic_first[T](xs []T) string {
	return \'\${xs[0]}\'
}

fn generic_line[T](xs []T) string {
	return \'node \${xs[0]}\'
}

fn check_typed_interpolation() {
	visited := {
		\'A\': true
	}
	visited_line := \'Visited: \${visited} done\'
	assert visited_line.contains(\'Visited: {\')
	assert visited_line.contains("\'A\': true")
	assert visited_line.contains(\'} done\')

	graph := {
		\'A\': [\'B\', \'C\']
	}
	graph_line := \'Graph: \${graph}\'
	assert graph_line.contains(\'Graph: {\')
	assert graph_line.contains("\'A\': [\'B\', \'C\']")

	int_names := {
		7: \'seven\'
	}
	int_names_line := \'Names: \${int_names}\'
	assert int_names_line.contains(\'Names: {\')
	assert int_names_line.contains("7: \'seven\'")

	nums := [1, 2]
	assert \'Array: \${nums}\' == \'Array: [1, 2]\'

	mut xs := []int{}
	xs << 7
	assert generic_first(xs) == \'7\'
	assert generic_line(xs) == \'node 7\'
}

fn main() {
	mut c := Counter{}
	assert \'\${c.next()}:\${c.next()}:\${c.next()}\' == \'1:2:3\'
	assert c.n == 3
	// Array and optional conversions introduce statements before the join.
	assert \'\${c.next()} / \${c.values()} / \${c.next()}\' == \'4 / [5] / 6\'
	assert \'\${c.next()} / \${c.maybe()} / \${c.next()}\' == \'7 / Option(8) / 9\'
	assert c.n == 9
	assert \'\${c.next():04d} / \${c.next():x} / \${c.next():3d}\' == \'0010 / b /  12\'
	assert c.next().str() + (c.next().str() + c.next().str()) == \'131415\'
	assert c.n == 15
	assert greeting == \'hello world!\'
	__v3_internal_symbol_join_0 := \'reserved\'
	assert \'<\${__v3_internal_symbol_join_0}>\' == \'<reserved>\'
	text := \'Привет\'
	assert decorate(text) == \'<Привет> / Привет\'
	zero := u8(0).ascii_str()
	joined := \'a\${zero}b\${text}\'
	assert joined.len == 3 + text.len
	assert joined[0] == 97 && joined[1] == 0 && joined[2] == 98
	assert joined[3..] == text
	assert text + zero + text == \'Привет\' + zero + \'Привет\'
	assert text == \'Привет\'
	check_typed_interpolation()
	println(\'ok\')
}
'
	bin := os.join_path(dir, 'v3_typed_and_joined_interpolation')
	compile := compile_string_interp_input(src, bin)
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'
}
