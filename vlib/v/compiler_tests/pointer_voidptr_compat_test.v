import os
import v.tempname

fn pointer_voidptr_run_good(dir string, name string, source string) string {
	src := os.join_path(dir, 'v3_${name}.v')
	os.write_file(src, source) or { panic(err) }
	bin := os.join_path(dir, 'v3_${name}')
	compile := os.exec([@VEXE, src, '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	return run.output.trim_space()
}

struct PointerReturnCase {
	name     string
	source   string
	expected string
}

fn pointer_voidptr_run_good_batch(dir string, entry PointerReturnCase, cases []PointerReturnCase) string {
	root := os.join_path(dir, 'pointer_returns')
	mut imports := []string{cap: cases.len}
	mut calls := []string{cap: cases.len}
	for case in cases {
		module_name := case.name
		module_dir := os.join_path(root, module_name)
		os.mkdir_all(module_dir) or { panic(err) }
		imports << 'import ${module_name}'
		calls << "\t\t'${case.name}' { ${module_name}.exercise(); return }"
		fixture := 'module ${module_name}\n\n' + case.source
		os.write_file(os.join_path(module_dir, 'v3_${case.name}.v'), fixture.replace_once('\nfn main()',
			'\npub fn exercise()')) or { panic(err) }
	}
	main := entry.source.replace_once('\nfn main() {\n', "\nfn main() {\n\tmatch os.args[1] {\n\t\t'${entry.name}' {}\n${calls.join('\n')}\n\t\telse { panic('unknown fixture') }\n\t}\n")
	os.write_file(os.join_path(root, 'main.v'), 'module main\n\nimport os\n${imports.join('\n')}\n\n' + main) or {
		panic(err)
	}
	bin := os.join_path(root, 'program')
	compile := os.exec([@VEXE, '-b', 'c', '-o', bin, root])
	assert compile.exit_code == 0, compile.output
	for case in [entry, ...cases] {
		run := os.exec([bin, case.name])
		assert run.exit_code == 0, '${case.name}: ${run.output}'
		assert run.output.trim_space() == case.expected, '${case.name}: expected `${case.expected}` in\n${run.output}'
	}
	return os.read_file(bin + '.c') or { panic(err) }
}

fn pointer_voidptr_run_bad(dir string, name string, source string, expected string) {
	src := os.join_path(dir, 'v3_${name}.v')
	os.write_file(src, source) or { panic(err) }
	bin := os.join_path(dir, 'v3_${name}')
	compile := os.exec([@VEXE, src, '-b', 'c', '-o', bin])
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains(expected), compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
}

fn test_pointer_return_lowering() {
	dir := os.join_path(os.vtmp_dir(), 'v3_pointer_voidptr_${tempname.unique_token()}')
	os.mkdir(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	// Keep the optional conversion and its type in the main module.
	optional := PointerReturnCase{
		name:     'optional_conversion'
		source:   'struct Item {
	value int
}

fn make() ?&Item {
	maybe := ?Item(Item{
		value: 47
	})
	converted := ?&Item(maybe)
	return converted
}

fn main() {
	item := make() or { return }
	println(int_str(item.value))
}
'
		expected: '47'
	}
	c_source := pointer_voidptr_run_good_batch(dir, optional, [
		PointerReturnCase{
			name:     'bare_pointer_return'
			source:   'struct Item {
	value int
}

fn make() &Item {
	item := Item{
		value: 23
	}
	return item
}

fn main() {
	item := make()
	println(int_str(item.value))
}
'
			expected: '23'
		},
		PointerReturnCase{
			name:     'bare_optional_pointer_return'
			source:   'struct Item {
	value int
}

fn make() ?&Item {
	item := Item{
		value: 29
	}
	defer {
		_ := 0
	}
	return item
}

fn main() {
	item := make() or { return }
	println(int_str(item.value))
}
'
			expected: '29'
		},
		PointerReturnCase{
			name:     'bare_result_pointer_return'
			source:   'struct Item {
	value int
}

fn make() !&Item {
	item := Item{
		value: 31
	}
	defer {
		_ := 0
	}
	return item
}

fn main() {
	item := make() or { return }
	println(int_str(item.value))
}
'
			expected: '31'
		},
		PointerReturnCase{
			name:     'wrapped_multi_return'
			source:   'struct Item {
	value int
}

fn make_option() ?(&Item, int) {
	item := Item{
		value: 31
	}
	return item, 37
}

fn make_result() !(&Item, int) {
	item := Item{
		value: 41
	}
	defer {
		_ := 0
	}
	return item, 43
}

fn main() {
	option_item, option_n := make_option() or { return }
	println(int_str(option_item.value))
	println(int_str(option_n))
	result_item, result_n := make_result() or { return }
	println(int_str(result_item.value))
	println(int_str(result_n))
}
'
			expected: '31\n37\n41\n43'
		},
		PointerReturnCase{
			name:     'voidptr_return'
			source:   'fn make_values() voidptr {
	unsafe {
		values := &voidptr(malloc(64))
		values[1] = voidptr(17)
		return values
	}
}

fn main() {
	unsafe {
		values := &voidptr(make_values())
		println(uint(values[1]))
		free(values)
	}
}
'
			expected: '17'
		},
	])
	assert c_source.contains('memdup(&maybe.value, sizeof(main__Item))'), c_source
	assert c_source.count('sizeof(wrapped_multi_return__Item)') >= 2, c_source
	assert !c_source.contains('return *values;'), c_source
	pointer_voidptr_run_bad(dir, 'bare_interface_pointer_return', 'interface Reader {
	read() int
}

struct File {}

fn (File) read() int {
	return 1
}

fn make() &Reader {
	file := File{}
	return file
}

fn main() {}
',
		'you are returning `File` instead')
	pointer_voidptr_run_bad(dir, 'bare_sum_pointer_return', 'struct A {}

struct B {}

type Item = A | B

fn make() &Item {
	value := A{}
	return value
}

fn main() {}
',
		'you are returning `A` instead')
}

fn test_c_voidptr_arguments_accept_struct_and_allocated_buffer() {
	dir := os.join_path(os.vtmp_dir(), 'v3_pointer_voidptr_${tempname.unique_token()}')
	os.mkdir(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	header := os.join_path(dir, 'v3_c_voidptr_arguments.h')
	os.write_file(header, 'static int v3_read_cfg(void *ptr) { return *((int *)ptr); }
static int v3_read_byte(void *ptr) { return *((unsigned char *)ptr); }
')!
	source := '#insert "${header}"

struct Cfg {
	value int
}

fn C.v3_read_cfg(voidptr) int
fn C.v3_read_byte(&voidptr) int

fn main() {
	cfg := Cfg{
		value: 17
	}
	println(int_str(C.v3_read_cfg(cfg)))

	buffer := vcalloc(1)
	unsafe {
		buffer[0] = 19
	}
	println(int_str(C.v3_read_byte(buffer)))
	unsafe {
		free(buffer)
	}
}
'
	out := pointer_voidptr_run_good(dir, 'c_voidptr_arguments', source)
	assert out == '17\n19', out
}
