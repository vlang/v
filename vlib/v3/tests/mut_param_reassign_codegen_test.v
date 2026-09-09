import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const vlib_dir = os.dir(v3_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')

fn mut_param_reassign_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_mut_param_reassign_codegen_test_${os.getpid()}')
	if os.is_executable(v3_bin) {
		return v3_bin
	}
	build :=
		os.execute('${vexe} -gc none -path "${vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn mut_param_reassign_run_good(v3_bin string, name string, source string) string {
	src := os.join_path(os.temp_dir(), 'v3_${name}_${os.getpid()}.v')
	os.write_file(src, source) or { panic(err) }
	bin := os.join_path(os.temp_dir(), 'v3_${name}_${os.getpid()}')
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	return run.output.trim_space()
}

fn mut_param_reassign_run_good_with_c(v3_bin string, name string, source string) (string, string) {
	src := os.join_path(os.temp_dir(), 'v3_${name}_${os.getpid()}.v')
	os.write_file(src, source) or { panic(err) }
	bin := os.join_path(os.temp_dir(), 'v3_${name}_${os.getpid()}')
	compile := os.execute('${v3_bin} ${src} -b c -keepc -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	c_source := os.read_file('${bin}.c') or { panic(err) }
	return run.output.trim_space(), c_source
}

fn mut_param_reassign_run_bad(v3_bin string, name string, source string, expected string) {
	src := os.join_path(os.temp_dir(), 'v3_${name}_${os.getpid()}.v')
	os.write_file(src, source) or { panic(err) }
	bin := os.join_path(os.temp_dir(), 'v3_${name}_${os.getpid()}')
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains(expected), compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
}

fn test_mut_array_param_reassigns_to_base_type() {
	v3_bin := mut_param_reassign_build_v3()
	out := mut_param_reassign_run_good(v3_bin, 'mut_array_param_reassign', 'fn replace(mut xs []int) {
	mut tmp := []int{}
	tmp << 4
	tmp << 9
	xs = tmp.clone()
}

fn main() {
	mut xs := []int{}
	xs << 1
	replace(mut xs)
	assert xs.len == 2
	assert xs[0] == 4
	assert xs[1] == 9
	println("ok")
}
')
	assert out == 'ok'
}

fn test_mut_string_param_concat_reads_as_string() {
	v3_bin := mut_param_reassign_build_v3()
	out := mut_param_reassign_run_good(v3_bin, 'mut_string_param_concat', "struct Text {
	value string
}

fn add(mut text Text) string {
	return text.value + '!'
}

fn main() {
	mut text := Text{
		value: 'hi'
	}
	println(add(mut text))
}
")
	assert out == 'hi!'
}

fn test_generic_mut_array_param_reassigns_to_base_type() {
	v3_bin := mut_param_reassign_build_v3()
	out := mut_param_reassign_run_good(v3_bin, 'generic_mut_array_param_reassign', 'struct Item {
	data int
	priority int
}

fn replace[T](mut xs []T, value T) {
	mut tmp := []T{}
	tmp << value
	xs = tmp.clone()
}

fn main() {
	mut xs := []Item{}
	replace(mut xs, Item{data: 7, priority: 3})
	assert xs.len == 1
	assert xs[0].data == 7
	assert xs[0].priority == 3
	println("ok")
}
')
	assert out == 'ok'
}

fn test_mut_map_param_reassigns_to_base_type() {
	v3_bin := mut_param_reassign_build_v3()
	out := mut_param_reassign_run_good(v3_bin, 'mut_map_param_reassign', "fn replace(mut m map[string]int) {
	tmp := {
		'answer': 42
	}
	m = tmp.clone()
}

fn main() {
	mut m := map[string]int{}
	m['old'] = 1
	replace(mut m)
	assert m.len == 1
	assert m['answer'] == 42
	println('ok')
}
")
	assert out == 'ok'
}

fn test_multi_return_assign_to_mut_array_param_uses_base_type() {
	v3_bin := mut_param_reassign_build_v3()
	out := mut_param_reassign_run_good(v3_bin, 'mut_array_param_multi_return_reassign', 'fn pair() ([]int, int) {
	return [1, 2], 7
}

fn replace(mut xs []int) {
	xs, _ = pair()
}

fn main() {
	mut xs := [0]
	replace(mut xs)
	assert xs.len == 2
	assert xs[0] == 1
	assert xs[1] == 2
	println("ok")
}
')
	assert out == 'ok'
}

fn test_inner_scope_redefinition_of_mut_param_is_rejected_before_c_compile() {
	v3_bin := mut_param_reassign_build_v3()
	mut_param_reassign_run_bad(v3_bin, 'bad_mut_array_param_inner_scope_redefinition', 'fn pair() ([]int, int) {
	return [1], 0
}

fn replace(mut xs []int) {
	if true {
		mut xs := 0
		_ = xs
	}
	if true {
		xs, _ = pair()
	}
}

fn main() {
	mut xs := [0]
	replace(mut xs)
	assert xs.len == 1
	assert xs[0] == 1
	println("ok")
}
',
		'redefinition of xs')
}

fn test_pointer_local_redefinition_of_mut_param_is_rejected_before_c_compile() {
	v3_bin := mut_param_reassign_build_v3()
	mut_param_reassign_run_bad(v3_bin, 'bad_mut_array_param_pointer_redefinition', 'fn f(mut xs []int) {
	mut local := []int{}
	mut other := []int{}
	{
		xs := &local
		xs = &other
		_ = xs
	}
}

fn main() {
	mut xs := [0]
	f(mut xs)
	assert xs.len == 1
	assert xs[0] == 0
	println("ok")
}
',
		'redefinition of xs')
}

fn test_mut_param_compound_assign_and_postfix_store_through_pointer() {
	v3_bin := mut_param_reassign_build_v3()
	out := mut_param_reassign_run_good(v3_bin, 'mut_param_compound_assign', 'struct Counter {
mut:
	value int
}

fn inc(mut counter Counter) {
	counter.value += 1
	counter.value++
}

fn main() {
	mut counter := Counter{
		value: 1
	}
	inc(mut counter)
	assert counter.value == 3
	println("ok")
}
')
	assert out == 'ok'
}

fn test_mut_param_unsigned_right_shift_assign_stores_through_pointer() {
	v3_bin := mut_param_reassign_build_v3()
	out := mut_param_reassign_run_good(v3_bin, 'mut_param_unsigned_right_shift_assign', 'struct Counter {
mut:
	value int
}

fn shift(mut counter Counter) {
	counter.value >>>= 1
}

fn main() {
	mut counter := Counter{
		value: 8
	}
	shift(mut counter)
	assert counter.value == 4
	println("ok")
}
')
	assert out == 'ok'
}

fn test_mut_pointer_param_reassigns_caller_slot() {
	v3_bin := mut_param_reassign_build_v3()
	out := mut_param_reassign_run_good(v3_bin, 'mut_pointer_param_reassign', 'struct Item {
	value int
}

fn replace(mut current &Item, replacement &Item) {
	current = replacement
}

fn main() {
	mut first := Item{
		value: 1
	}
	second := Item{
		value: 9
	}
	mut current := &first
	replace(mut current, &second)
	assert current == &second
	assert first.value == 1
	println(int_str(current.value))
}
')
	assert out == '9'
	out_generic := mut_param_reassign_run_good(v3_bin, 'generic_mut_pointer_param_reassign', 'struct Item {
	value int
}

fn replace[T](mut current &T, replacement &T) {
	current = replacement
}

fn main() {
	mut first := Item{
		value: 2
	}
	second := Item{
		value: 8
	}
	mut current := &first
	replace(mut current, &second)
	println(int_str(current.value))
}
')
	assert out_generic == '8'
	out_lvalues := mut_param_reassign_run_good(v3_bin, 'mut_pointer_lvalue_slots', 'struct Item {
	value int
}

struct Holder {
mut:
	current &Item
}

fn replace(mut current &Item, replacement &Item) {
	current = replacement
}

fn main() {
	mut first := Item{
		value: 1
	}
	second := Item{
		value: 9
	}
	third := Item{
		value: 7
	}
	mut items := [&first]
	mut holder := Holder{
		current: &first
	}
	replace(mut items[0], &second)
	replace(mut holder.current, &third)
	println(int_str(items[0].value))
	println(int_str(holder.current.value))
}
')
	assert out_lvalues == '9\n7'
	out_forwarded := mut_param_reassign_run_good(v3_bin, 'mut_pointer_param_forward_and_index', 'fn write_byte(mut bytes &u8, value u8) {
	unsafe {
		bytes[0] = value
	}
}

fn read_byte(mut bytes &u8) u8 {
	return *bytes
}

fn terminate(mut bytes &u8) {
	write_byte(mut bytes, `Z`)
	unsafe {
		bytes[1] = 0
	}
}

fn main() {
	mut storage := [2]u8{}
	mut bytes := unsafe { &storage[0] }
	terminate(mut bytes)
	assert storage[0] == `Z`
	assert storage[1] == 0
	assert read_byte(mut bytes) == `Z`
	println("ok")
}
')
	assert out_forwarded == 'ok'
	mut_param_reassign_run_bad(v3_bin, 'generic_mut_pointer_param_requires_pointer_slot', 'struct Item {
	value int
}

fn replace[T](mut current &T, replacement &T) {
	current = replacement
}

fn main() {
	mut current := Item{
		value: 2
	}
	replacement := Item{
		value: 8
	}
	replace[Item](mut current, &replacement)
}
',
		'expected `&&Item`')
}

fn test_fn_literal_mut_pointer_param_reassigns_caller_slot() {
	v3_bin := mut_param_reassign_build_v3()
	out := mut_param_reassign_run_good(v3_bin, 'fn_literal_mut_pointer_param_reassign', 'struct Item {
	value int
}

fn main() {
	mut first := Item{
		value: 1
	}
	second := Item{
		value: 9
	}
	mut current := &first
	replace := fn (mut current &Item, replacement &Item) {
		current = replacement
	}
	replace(mut current, &second)
	assert current == &second
	assert first.value == 1
	println(int_str(current.value))
}
')
	assert out == '9'
}

fn test_fn_literal_value_param_shadow_preserves_outer_pointer_flags() {
	v3_bin := mut_param_reassign_build_v3()
	out := mut_param_reassign_run_good(v3_bin, 'fn_literal_value_param_shadow', 'struct Item {
	value int
}

fn replace_after_shadow(mut current &Item, replacement &Item) {
	read := fn (current int) int {
		return current
	}
	assert read(7) == 7
	current = replacement
}

fn main() {
	mut first := Item{
		value: 1
	}
	second := Item{
		value: 9
	}
	mut current := &first
	replace_after_shadow(mut current, &second)
	assert current == &second
	assert first.value == 1
	println(int_str(current.value))
}
')
	assert out == '9'
}

fn test_mut_pointer_param_signature_and_expression_conversions() {
	v3_bin := mut_param_reassign_build_v3()
	out, c_source := mut_param_reassign_run_good_with_c(v3_bin,
		'mut_pointer_param_signature_and_expression_conversions', 'interface Reader {
	read() int
}

struct Item {
	value int
}

fn (item &Item) read() int {
	return item.value
}

fn consume_reader(reader Reader) int {
	return reader.read()
}

fn consume_pointer(item &Item) int {
	return item.value
}

fn consume_optional(item ?&Item) int {
	if value := item {
		return value.value
	}
	return 0
}

fn read_field(mut item &Item) int {
	return item.value
}

fn read_deref(mut item &Item) int {
	return (*item).value
}

fn copy_deref(mut item &Item) Item {
	return *item
}

fn assign_deref(mut item &Item) Item {
	mut copied_value := Item{}
	copied_value = *item
	return copied_value
}

fn increment_deref(mut value &int) int {
	(*value)++
	return *value
}

fn read_method(mut item &Item) int {
	return item.read()
}

fn forward(mut item &Item) int {
	copied := copy_deref(mut item)
	assigned := assign_deref(mut item)
	mut number_value := 5
	mut number := &number_value
	incremented := increment_deref(mut number)
	return read_field(mut item) + read_deref(mut item) + read_method(mut item) + consume_reader(item) + consume_pointer(item) + consume_optional(item) + copied.value + assigned.value + incremented
}

fn main() {
	mut value := Item{
		value: 7
	}
	mut item := &value
	println(int_str(forward(mut item)))
}
')
	assert out == '62'
	assert c_source.contains('i64 read_field(main__Item** item) {'), 'missing main__Item** signature'
	assert !c_source.contains('i64 read_field(main__Item*** item) {'), 'found over-indirected main__Item*** signature'
	assert c_source.contains('return ((*item))->value;'), 'missing single slot dereference'
	assert c_source.contains('return (*(*item));'), 'missing source dereference after slot dereference'
	assert c_source.contains('copied_value = (*(*item));'), 'missing standalone assignment dereference'
	assert c_source.contains('((*(*value)))++;'), 'missing standalone postfix dereference'
}

fn test_generic_mut_sum_parameter_forwards_existing_pointer() {
	v3_bin := mut_param_reassign_build_v3()
	out, c_source := mut_param_reassign_run_good_with_c(v3_bin, 'generic_mut_sum_forward', 'struct Cat {
	name string
}

struct Dog {
	name string
}

type Animal = Cat | Dog

fn replace[T](mut value T, replacement T) {
	value = replacement
}

fn decode[T](mut value T, replacement T) {
	replace(mut value, replacement)
}

fn main() {
	mut animal := Animal(Dog{
		name: "Rex"
	})
	decode(mut animal, Animal(Cat{
		name: "Tom"
	}))
	println(animal)
}
')
	assert out == "Animal(Cat{\n    name: 'Tom'\n})"
	assert c_source.contains('replace_T_Animal(value, replacement);')
	assert !c_source.contains('replace_T_Animal(&value, replacement);')
}

fn test_mut_param_reassign_keeps_invalid_assignments_rejected() {
	v3_bin := mut_param_reassign_build_v3()
	mut_param_reassign_run_bad(v3_bin, 'bad_same_scope_mut_string_param_redeclare', "struct Text {
	value string
}

fn shadow_read(mut text Text) string {
	mut text := Text{
		value: 'local'
	}
	return text.value + '!'
}

fn main() {
	mut text := Text{
		value: 'param'
	}
	_ = shadow_read(mut text)
}
",
		'redefinition of text')
	mut_param_reassign_run_bad(v3_bin, 'bad_mut_array_param_reassign_elem', "fn bad(mut xs []int) {
	mut ys := []string{}
	ys << 'bad'
	xs = ys
}

fn main() {
	mut xs := []int{}
	bad(mut xs)
}
",
		'expected `[]int`, not `[]string`')
	mut_param_reassign_run_bad(v3_bin, 'bad_mut_array_param_reassign_scalar', 'fn bad(mut xs []int) {
	xs = 1
}

fn main() {
	mut xs := []int{}
	bad(mut xs)
}
',
		'expected `[]int`, not `int literal`')
	mut_param_reassign_run_bad(v3_bin, 'bad_pointer_local_reassign_value', 'fn main() {
	mut xs := []int{}
	mut p := &xs
	mut tmp := []int{}
	p = tmp
}
',
		'expected `&[]int`, not `[]int`')
	mut_param_reassign_run_bad(v3_bin, 'bad_shadowed_mut_param_multi_return', 'fn pair() ([]int, int) {
	mut xs := []int{}
	return xs, 7
}

fn replace(mut xs []int) {
	if true {
		mut xs := 0
		xs, _ = pair()
	}
}

fn main() {
	mut xs := []int{}
	replace(mut xs)
}
',
		'redefinition of xs')
}
