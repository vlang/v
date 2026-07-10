import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const vlib_dir = os.dir(v3_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')

fn string_interp_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_string_interpolation_stringify_test_${os.getpid()}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${vexe} -gc none -path "${vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn compile_v3_input(v3_bin string, name string, source string, bin string) os.Result {
	src := os.join_path(os.temp_dir(), '${name}_${os.getpid()}.v')
	os.write_file(src, source) or { panic(err) }
	return os.execute('${v3_bin} ${src} -b c -o ${bin}')
}

fn test_typed_string_interpolation_stringifies_maps_arrays_and_generic_indexes() {
	v3_bin := string_interp_build_v3()
	src := 'fn generic_first[T](xs []T) string {
	return \'\${xs[0]}\'
}

fn generic_line[T](xs []T) string {
	return \'node \${xs[0]}\'
}

fn main() {
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
	println(\'ok\')
}
'
	bin := os.join_path(os.temp_dir(), 'v3_string_interpolation_stringify_positive_${os.getpid()}')
	compile := compile_v3_input(v3_bin, 'v3_string_interpolation_stringify_positive', src, bin)
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'
}

fn test_string_interpolation_method_call_marks_callee_used() {
	v3_bin := string_interp_build_v3()
	src := 'struct Begin {
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

fn main() {
	msg := Message{
		begin: Begin{
			value: 7
		}
	}
	println(msg.to_json())
}
'
	bin := os.join_path(os.temp_dir(), 'v3_string_interp_method_markused_${os.getpid()}')
	compile := compile_v3_input(v3_bin, 'v3_string_interp_method_markused', src, bin)
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '{"begin":7}'
}

fn test_string_interpolation_discovered_str_method_emits_helpers() {
	v3_bin := string_interp_build_v3()
	src := "struct Wrap {}

fn str_helper() string {
	return 'wrapped'
}

fn (w Wrap) str() string {
	_ = w
	return str_helper()
}

fn main() {
	println('\${Wrap{}}')
}
"
	bin := os.join_path(os.temp_dir(), 'v3_string_interp_str_helper_${os.getpid()}')
	compile := compile_v3_input(v3_bin, 'v3_string_interp_str_helper', src, bin)
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'wrapped'
}

fn test_defer_string_interpolation_with_generic_type_arguments() {
	v3_bin := string_interp_build_v3()
	src := "struct Box[T] {
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
	mut rune_value_lookup := map[PlainBox[T]]rune{}
	rune_value_lookup[b] = `y`
	defer {
		println('plain \${b} \${list} \${lookup} \${rune_lookup} \${rune_value_lookup}')
	}
	println('body-plain')
}

fn show_value[D](value D) {
	defer {
		println('value \${value}')
	}
	println('body-d')
}

fn main() {
	show_box(Box[int]{
		value: 7
	})
	show_plain_box(PlainBox[int]{
		value: 8
	})
	show_value(9)
}
"
	bin := os.join_path(os.temp_dir(), 'v3_defer_generic_string_interp_${os.getpid()}')
	compile := compile_v3_input(v3_bin, 'v3_defer_generic_string_interp', src, bin)
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	expected := "body-box
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
}} {PlainBox[int]{
    value: 8
}: `y`}
body-d
value 9"
	assert run.output.trim_space() == expected
}

fn test_string_plus_accepts_string_aliases() {
	v3_bin := string_interp_build_v3()
	src := "type MyString = string

fn main() {
	s := MyString('y')
	assert 'x' + s == 'xy'
	assert s + 'z' == 'yz'
	println('ok')
}
"
	bin := os.join_path(os.temp_dir(), 'v3_string_alias_plus_positive_${os.getpid()}')
	compile := compile_v3_input(v3_bin, 'v3_string_alias_plus_positive', src, bin)
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'
}

fn test_primitive_alias_chains_stringify_without_alias_wrapper() {
	v3_bin := string_interp_build_v3()
	src := "type A = string
type B = A
type I = int
type J = I
type Truth = bool
type Toggle = Truth

fn main() {
	println(B('x'))
	println(J(7))
	println(Toggle(true))
}
"
	bin := os.join_path(os.temp_dir(), 'v3_primitive_alias_chain_str_${os.getpid()}')
	compile := compile_v3_input(v3_bin, 'v3_primitive_alias_chain_str', src, bin)
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'x\n7\ntrue'
}

fn test_string_plus_does_not_stringify_non_strings() {
	v3_bin := string_interp_build_v3()
	int_bin := os.join_path(os.temp_dir(), 'v3_string_plus_int_negative_${os.getpid()}')
	int_result := compile_v3_input(v3_bin, 'v3_string_plus_int_negative', "fn main() {
	_ := 'x' + 1
}
",
		int_bin)
	assert int_result.exit_code != 0, int_result.output
	assert int_result.output.contains('operator `+` cannot concatenate `string` and `int`'), int_result.output
	assert !int_result.output.contains('C compilation failed'), int_result.output

	map_bin := os.join_path(os.temp_dir(), 'v3_string_plus_map_negative_${os.getpid()}')
	map_result := compile_v3_input(v3_bin, 'v3_string_plus_map_negative', "fn main() {
	_ := 'x' + map[string]int{}
}
",
		map_bin)
	assert map_result.exit_code != 0, map_result.output
	assert map_result.output.contains('operator `+` cannot concatenate `string` and `map[string]int`'), map_result.output
	assert !map_result.output.contains('C compilation failed'), map_result.output
}
