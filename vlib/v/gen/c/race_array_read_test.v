module c

import os
import v.cmdexec

fn test_race_array_value_reads_preserve_arguments() ! {
	$if !(linux || macos || freebsd || netbsd) {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'race_array_read_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'struct Cell {
mut:
	values []int
}
struct Counter {
mut:
	value int
}
fn first(values []int) int { return values[0] }
fn copy_values(mut dst []int, cell &Cell) { copy(mut dst, cell.values) }
fn read_value(cell &Cell) int { return first(cell.values) }
fn read_element(cell &Cell) int { return cell.values[0] }
fn next_index(mut calls Counter) int { calls.value++; return 0 }
fn read_indexed(values [][]int, mut calls Counter) int {
	return first(values[next_index(mut calls)])
}
type Values = []int
fn read_alias(values Values) int { return first(values) }
fn read_reference(values &[]int) int { return first(*values) }
fn read_slice(cell &Cell) []int { return cell.values[1..2] }
fn read_gated_slice(cell &Cell) []int { return cell.values#[1..20] }
fn read_slice_from_start(cell &Cell) []int { return cell.values[..1] }
fn read_slice_to_end(cell &Cell) []int { return cell.values[1..] }
fn read_slice_all(cell &Cell) []int { return cell.values[..] }
fn read_slice_alias(values Values) []int { return values[0..1] }
fn read_slice_reference(values &[]int) []int { return (*values)[0..1] }
fn read_slice_pointer(values &[]int) []int { return values[0..1] }
fn read_slice_indexed(values [][]int, mut calls Counter) []int {
	return values[next_index(mut calls)][1..2]
}
type OverloadedLists = [][]int
type OverloadedFixedLists = [1][]int
fn (values OverloadedLists) [] (index int) []int { return [42] }
fn (values OverloadedFixedLists) [] (index int) []int { return [43] }
fn read_overloaded(values OverloadedLists, mut calls Counter) int {
	return first(values[next_index(mut calls)])
}
fn read_fixed_overloaded(values OverloadedFixedLists, mut calls Counter) int {
	return first(values[next_index(mut calls)])
}
fn returned_values() []int { return [6] }
fn read_temporary() int { return first(returned_values()) }
fn returned_cell() Cell { return Cell{values: [7]} }
fn read_temporary_selector() int { return first(returned_cell().values) }
fn read_slice_temporary() []int { return returned_values()[0..1] }
fn read_slice_temporary_selector() []int { return returned_cell().values[0..1] }
fn read_slice_fixed(values [2]int) []int { return values[1..2] }
fn change_values(mut values []int) { values[0] = 9 }
fn forward_mut(mut cell Cell) { change_values(mut cell.values) }
fn pointer_len(values &[]int) int { return values.len }
fn forward_pointer(values &[]int) int { return pointer_len(values) }
fn fixed_first(values [2]int) int { return values[0] }
fn forward_fixed(values [2]int) int { return fixed_first(values) }
fn main() {
	cell := Cell{values: [2, 3]}
	mut dst := []int{len: 2}
	copy_values(mut dst, &cell)
	assert dst == [2, 3]
	assert read_value(&cell) == 2
	assert read_element(&cell) == 2
	mut calls := Counter{}
	assert read_indexed([[4]], mut calls) == 4
	assert calls.value == 1
	assert read_alias(Values([5])) == 5
	assert read_reference(&cell.values) == 2
	assert read_slice(&cell) == [3]
	assert read_gated_slice(&cell) == [3]
	assert read_slice_from_start(&cell) == [2]
	assert read_slice_to_end(&cell) == [3]
	assert read_slice_all(&cell) == [2, 3]
	assert read_slice_alias(Values([5])) == [5]
	assert read_slice_reference(&cell.values) == [2]
	assert read_slice_pointer(&cell.values) == [2]
	mut slice_calls := Counter{}
	assert read_slice_indexed([[4, 5]], mut slice_calls) == [5]
	assert slice_calls.value == 1
	mut overloaded_calls := Counter{}
	assert read_overloaded(OverloadedLists([[0]]), mut overloaded_calls) == 42
	assert overloaded_calls.value == 1
	mut fixed_calls := Counter{}
	assert read_fixed_overloaded(OverloadedFixedLists([[0]]!), mut fixed_calls) == 43
	assert fixed_calls.value == 1
	assert read_temporary() == 6
	assert read_temporary_selector() == 7
	assert read_slice_temporary() == [6]
	assert read_slice_temporary_selector() == [7]
	assert read_slice_fixed([8, 9]!) == [9]
	mut changed := Cell{values: [1]}
	forward_mut(mut changed)
	assert changed.values == [9]
	assert forward_pointer(&cell.values) == 2
	assert forward_fixed([8, 9]!) == 8
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
		for name in ['copy_values', 'read_value', 'read_element', 'read_indexed', 'read_alias',
			'read_reference'] {
			body := race_array_function_body(output, name)
			assert body.contains('__tsan_read_range((void*)') == race, '${name}: ${body}'
			if race && name in ['copy_values', 'read_value', 'read_element'] {
				assert body.contains('&(cell->values)'), body
			}
		}
		for name in ['read_slice', 'read_gated_slice', 'read_slice_from_start', 'read_slice_to_end',
			'read_slice_all', 'read_slice_alias', 'read_slice_reference', 'read_slice_indexed'] {
			body := race_array_function_body(output, name)
			helper := if name == 'read_gated_slice' { 'array__slice_ni' } else { 'array_slice' }
			// The sliced header must be annotated, not just the outer array in an index.
			assert body.contains('${helper}(({ const Array* ') == race, '${name}: ${body}'
			if race && name in ['read_slice', 'read_gated_slice', 'read_slice_from_start',
				'read_slice_to_end', 'read_slice_all'] {
				assert body.contains('&(cell->values)'), body
			}
		}
		for name in ['read_indexed', 'read_overloaded', 'read_fixed_overloaded', 'read_slice_indexed'] {
			body := race_array_function_body(output, name)
			assert body.count('next_index(') == 1, body
		}
		for name in ['read_temporary', 'read_temporary_selector', 'forward_mut', 'forward_pointer',
			'forward_fixed', 'read_slice_temporary', 'read_slice_temporary_selector', 'read_slice_fixed'] {
			body := race_array_function_body(output, name)
			assert !body.contains('__tsan_read_range((void*)'), '${name}: ${body}'
		}
		// Overloaded indexes and selectors on call results produce C rvalues.
		// The instrumentation must not try to take their addresses.
		checked := cmdexec.run('cc', ['-std=gnu11', '-w', '-fsyntax-only', c_file])
		assert checked.exit_code == 0, checked.output
	}
	// Verify the fixture's values, mut forwarding, and single-evaluation assertions
	// without requiring a working ThreadSanitizer runtime on every test host.
	ran := cmdexec.run(@VEXE, ['-nocache', '-cc', 'cc', '-no-retry-compilation', 'run', source])
	assert ran.exit_code == 0, ran.output
}

fn race_array_function_body(source string, name string) string {
	lines := source.split_into_lines()
	for i, line in lines {
		if line.ends_with(' {') && line.all_before('(').all_after_last(' ').ends_with(name) {
			return lines[i + 1..].join('\n').all_before('\n}')
		}
	}
	panic('missing generated function ${name}')
}
