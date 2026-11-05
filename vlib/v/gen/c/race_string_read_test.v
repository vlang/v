module c

import os
import v.cmdexec

fn test_race_string_operations_read_the_original_header_once() ! {
	$if !(linux || macos || freebsd || netbsd) {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'race_string_read_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'const constant_string = "hello"
struct Cell {
mut:
	value string
}
struct Counter {
mut:
	value int
}
fn read_concat(cell &Cell) string { return cell.value + "!" }
fn read_compare(cell &Cell, other string) bool { return cell.value == other }
fn read_slice(cell &Cell) string { return cell.value[1..3] }
fn next_index(mut calls Counter) int { calls.value++; return 0 }
fn read_indexed(values []string, mut calls Counter) string {
	return values[next_index(mut calls)] + "!"
}
fn returned_string() string { return "hello" }
fn read_temporary() string { return returned_string() + "!" }
fn returned_cell() Cell { return Cell{value: "hello"} }
fn read_temporary_selector() string { return returned_cell().value + "!" }
fn read_map(values map[string]string) string { return values["key"] + "!" }
fn next_key(mut calls Counter) string { calls.value++; return "key" }
fn read_indexed_map(values map[string]string, mut calls Counter) string {
	return values[next_key(mut calls)] + "!"
}
fn read_const() string { return constant_string + "!" }
fn read_optional(value ?string) string { return (value or { "fallback" }) + "!" }
fn read_reference(value &string) string { return *value + "!" }
fn replace_value(mut cell Cell) { cell.value = "world" }
fn forward_mut(mut cell Cell) { replace_value(mut cell) }
fn main() {
	cell := Cell{value: "hello"}
	assert read_concat(&cell) == "hello!"
	assert read_compare(&cell, "hello")
	assert read_slice(&cell) == "el"
	mut calls := Counter{}
	assert read_indexed(["hello"], mut calls) == "hello!"
	assert calls.value == 1
	assert read_temporary() == "hello!"
	assert read_temporary_selector() == "hello!"
	assert read_map({"key": "hello"}) == "hello!"
	assert read_map(map[string]string{}) == "!"
	mut key_calls := Counter{}
	assert read_indexed_map({"key": "hello"}, mut key_calls) == "hello!"
	assert key_calls.value == 1
	assert read_const() == "hello!"
	assert read_optional("hello") == "hello!"
	assert read_optional(none) == "fallback!"
	plain := "hello"
	assert read_reference(&plain) == "hello!"
	mut value := Cell{value: "hello"}
	forward_mut(mut value)
	assert value.value == "world"
}
')!
	for race in [false, true] {
		c_file := os.join_path(root, if race { 'race.c' } else { 'normal.c' })
		mut flags := ['-nocache', '-cc', 'cc', '-no-retry-compilation', '-o', c_file]
		if race {
			flags << '-race'
		}
		generated := cmdexec.run(@VEXE, [...flags, source])
		assert generated.exit_code == 0, generated.output
		output := os.read_file(c_file)!
		for name in ['read_concat', 'read_compare', 'read_slice', 'read_indexed'] {
			body := race_string_function_body(output, name)
			assert body.contains('__tsan_read_range((void*)') == race, '${name}: ${body}'
			if race && name != 'read_indexed' {
				assert body.replace('\t', '').contains('&(cell->value)'), body
			}
		}
		indexed := race_string_function_body(output, 'read_indexed')
		assert indexed.count('next_index(') == 1, indexed
		indexed_map := race_string_function_body(output, 'read_indexed_map')
		assert indexed_map.count('next_key(') == 1, indexed_map
		for name in ['read_temporary', 'read_temporary_selector', 'forward_mut'] {
			body := race_string_function_body(output, name)
			assert !body.contains('__tsan_read_range((void*)'), '${name}: ${body}'
		}
		if race {
			// Lowered map lookups can expose real storage through a pointer, while
			// calls and temporary selectors must never be addressed as C rvalues.
			checked := cmdexec.run('cc', ['-std=gnu11', '-w', '-fsyntax-only', c_file])
			assert checked.exit_code == 0, checked.output
		}
	}
}

fn race_string_function_body(source string, name string) string {
	lines := source.split_into_lines()
	for i, line in lines {
		if line.ends_with(' {') && line.all_before('(').all_after_last(' ').ends_with(name) {
			return lines[i + 1..].join('\n').all_before('\n}')
		}
	}
	panic('missing generated function ${name}')
}
