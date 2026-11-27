import json2
import os
import v.tempname

fn mut_param_reassign_fixture_dir() string {
	dir := os.join_path(os.vtmp_dir(), 'v3_mut_param_reassign_${tempname.unique_token()}')
	os.mkdir(dir) or { panic(err) }
	return dir
}

fn mut_param_reassign_run_good_with_c(name string, source string) (string, string) {
	dir := mut_param_reassign_fixture_dir()
	defer {
		os.rmdir_all(dir) or {}
	}
	bin := os.join_path(dir, 'v3_${name}')
	src := bin + '.v'
	os.write_file(src, source) or { panic(err) }
	compile := os.exec([@VEXE, src, '-b', 'c', '-keepc', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	c_source := os.read_file('${bin}.c') or { panic(err) }
	return run.output.trim_space(), c_source
}

struct MutParamReassignCase {
	name     string
	source   string
	expected string
}

struct MutParamReassignDiagnostic {
	file     string
	severity string
	message  string
}

fn mut_param_reassign_write_module(dir string, module_name string, name string, source string) {
	module_dir := os.join_path(dir, module_name)
	os.mkdir(module_dir) or { panic(err) }
	os.write_file(os.join_path(module_dir, '${name}.v'), 'module ${module_name}\n\n' + source) or {
		panic(err)
	}
}

fn mut_param_reassign_run_good_batch(cases []MutParamReassignCase) {
	dir := mut_param_reassign_fixture_dir()
	defer { os.rmdir_all(dir) or {} }
	mut imports := []string{cap: cases.len}
	mut calls := []string{cap: cases.len}
	for i, case in cases {
		module_name := 'fixture_${i}'
		imports << 'import ${module_name}'
		calls << "\t\t'${case.name}' { ${module_name}.exercise() }"
		source := case.source.replace_once('\nfn main()', '\npub fn exercise()')
		mut_param_reassign_write_module(dir, module_name, case.name, source)
	}
	os.write_file(os.join_path(dir, 'main.v'), "module main\n\nimport os\n${imports.join('\n')}\n\nfn main() {\n\tmatch os.args[1] {\n${calls.join('\n')}\n\t\telse { panic('unknown fixture') }\n\t}\n}\n") or {
		panic(err)
	}
	bin := os.join_path(dir, 'program')
	compile := os.exec([@VEXE, '-b', 'c', '-o', bin, dir])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	for case in cases {
		run := os.exec([bin, case.name])
		assert run.exit_code == 0, '${case.name}: ${run.output}'
		assert run.output.trim_space() == case.expected, '${case.name}: expected `${case.expected}` in\n${run.output}'
	}
}

fn mut_param_reassign_run_bad_batch(cases []MutParamReassignCase) {
	dir := mut_param_reassign_fixture_dir()
	defer { os.rmdir_all(dir) or {} }
	mut imports := []string{cap: cases.len}
	for case in cases {
		module_name := case.name
		imports << 'import ${module_name} as _'
		mut_param_reassign_write_module(dir, module_name, case.name, case.source)
	}
	os.write_file(os.join_path(dir, 'main.v'), 'module main\n\n${imports.join('\n')}\n\nfn main() {}\n') or {
		panic(err)
	}
	compile := os.exec([@VEXE, '-json-errors', '-b', 'c', '-o', os.join_path(dir, 'program'), dir])
	assert compile.exit_code != 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	mut errors := map[string][]string{}
	for line in compile.output.split_into_lines() {
		if !line.starts_with('{') {
			continue
		}
		diagnostic := json2.decode[MutParamReassignDiagnostic](line) or { panic(err) }
		if diagnostic.severity == 'error' {
			errors[os.file_name(diagnostic.file)] << diagnostic.message
		}
	}
	for case in cases {
		assert errors['${case.name}.v'].any(it.contains(case.expected)), '${case.name}: expected `${case.expected}` in\n${compile.output}'
	}
}

fn test_mut_param_reassign_runtime_cases() {
	mut_param_reassign_run_good_batch([
		MutParamReassignCase{
			name:     'mut_array_param_reassign'
			source:   'fn replace(mut xs []int) {
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
'
			expected: 'ok'
		},
		MutParamReassignCase{
			name:     'mut_string_param_concat'
			source:   "struct Text {
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
"
			expected: 'hi!'
		},
		MutParamReassignCase{
			name:     'generic_mut_array_param_reassign'
			source:   'struct Item {
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
'
			expected: 'ok'
		},
		MutParamReassignCase{
			name:     'mut_map_param_reassign'
			source:   "fn replace(mut m map[string]int) {
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
"
			expected: 'ok'
		},
		MutParamReassignCase{
			name:     'mut_array_param_multi_return_reassign'
			source:   'fn pair() ([]int, int) {
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
'
			expected: 'ok'
		},
		MutParamReassignCase{
			name:     'mut_param_compound_assign'
			source:   'struct Counter {
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
'
			expected: 'ok'
		},
		MutParamReassignCase{
			name:     'mut_param_unsigned_right_shift_assign'
			source:   'struct Counter {
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
'
			expected: 'ok'
		},
		MutParamReassignCase{
			name:     'mut_pointer_param_reassign'
			source:   'struct Item {
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
'
			expected: '9'
		},
		MutParamReassignCase{
			name:     'generic_mut_pointer_param_reassign'
			source:   'struct Item {
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
'
			expected: '8'
		},
		MutParamReassignCase{
			name:     'mut_pointer_lvalue_slots'
			source:   'struct Item {
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
'
			expected: '9\n7'
		},
		MutParamReassignCase{
			name:     'mut_pointer_param_forward_and_index'
			source:   'fn write_byte(mut bytes &u8, value u8) {
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
'
			expected: 'ok'
		},
		MutParamReassignCase{
			name:     'fn_literal_mut_pointer_param_reassign'
			source:   'struct Item {
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
'
			expected: '9'
		},
		MutParamReassignCase{
			name:     'fn_literal_value_param_shadow'
			source:   'struct Item {
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
'
			expected: '9'
		},
	])
}

fn test_mut_parameter_pointer_slots_and_forwarding_codegen() {
	out, c_source := mut_param_reassign_run_good_with_c('mut_param_pointer_codegen', 'interface Reader {
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

fn check_pointer_slot_conversions() {
	mut value := Item{
		value: 7
	}
	mut item := &value
	println(int_str(forward(mut item)))
}

struct Cat {
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
	check_pointer_slot_conversions()
	mut animal := Animal(Dog{
		name: "Rex"
	})
	decode(mut animal, Animal(Cat{
		name: "Tom"
	}))
	println(animal)
}
')
	assert out == "62\nAnimal(Cat{\n    name: 'Tom'\n})"
	assert c_source.contains('i64 read_field(main__Item** item) {'), 'missing main__Item** signature'
	assert !c_source.contains('i64 read_field(main__Item*** item) {'), 'found over-indirected main__Item*** signature'
	read_field_c := c_source.all_after('i64 read_field(main__Item** item) {').all_before('\n}')
	copy_deref_c := c_source.all_after('main__Item copy_deref(main__Item** item) {').all_before('\n}')
	assign_deref_c := c_source.all_after('main__Item assign_deref(main__Item** item) {').all_before('\n}')
	increment_deref_c := c_source.all_after('i64 increment_deref(i64** value) {').all_before('\n}')
	assert read_field_c.contains('return ((*item))->value;'), 'missing single slot dereference'
	assert copy_deref_c.contains('return (*(*item));'), 'missing source dereference after slot dereference'
	assert assign_deref_c.contains('copied_value = (*(*item));'), 'missing standalone assignment dereference'
	assert increment_deref_c.contains('((*(*value)))++;'), 'missing standalone postfix dereference'

	decode_c := c_source.all_after('void decode_T_Animal(Animal* value, Animal* replacement) {').all_before('\n}')
	assert decode_c.contains('replace_T_Animal(value, '), decode_c
	assert !decode_c.contains('replace_T_Animal(&value, '), decode_c
}

fn test_mut_param_reassign_keeps_invalid_assignments_rejected() {
	pointer_slot_case := 'generic_mut_pointer_param_requires_pointer_slot'
	pointer_slot_ref := '&${pointer_slot_case}.Item'
	mut_param_reassign_run_bad_batch([
		MutParamReassignCase{
			name:     'bad_mut_array_param_inner_scope_redefinition'
			source:   'fn pair() ([]int, int) {
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
'
			expected: 'redefinition of `xs`'
		},
		MutParamReassignCase{
			name:     'bad_mut_array_param_pointer_redefinition'
			source:   'fn f(mut xs []int) {
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
'
			expected: 'redefinition of `xs`'
		},
		MutParamReassignCase{
			name:     pointer_slot_case
			source:   'struct Item {
	value int
}

fn replace_pointer_slot[T](mut current &T, replacement &T) {
	current = replacement
}

fn main() {
	mut current := Item{
		value: 2
	}
	replacement := Item{
		value: 8
	}
	replace_pointer_slot[Item](mut current, &replacement)
}
'
			expected: 'parameter `current` is `mut current ${pointer_slot_ref}`, a mutable reference to a `${pointer_slot_ref}`'
		},
		MutParamReassignCase{
			name:     'bad_same_scope_mut_string_param_redeclare'
			source:   "struct Text {
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
"
			expected: 'redefinition of `text`'
		},
		MutParamReassignCase{
			name:     'bad_mut_array_param_reassign_elem'
			source:   "fn bad(mut xs []int) {
	mut ys := []string{}
	ys << 'bad'
	xs = ys
}

fn main() {
	mut xs := []int{}
	bad(mut xs)
}
"
			expected: 'expected `[]int`, not `[]string`'
		},
		MutParamReassignCase{
			name:     'bad_mut_array_param_reassign_scalar'
			source:   'fn bad(mut xs []int) {
	xs = 1
}

fn main() {
	mut xs := []int{}
	bad(mut xs)
}
'
			expected: 'expected `[]int`, not `int literal`'
		},
		MutParamReassignCase{
			name:     'bad_pointer_local_reassign_value'
			source:   'fn main() {
	mut xs := []int{}
	mut p := &xs
	mut tmp := []int{}
	p = tmp
}
'
			expected: 'expected `&[]int`, not `[]int`'
		},
		MutParamReassignCase{
			name:     'bad_shadowed_mut_param_multi_return'
			source:   'fn pair() ([]int, int) {
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
'
			expected: 'redefinition of `xs`'
		},
	])
}
