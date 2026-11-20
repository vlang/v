module c

import os
import v.cmdexec

// `_ = value` must tell the race detector that the storage of a struct value is read. A copy
// of the struct is not enough: the ThreadSanitizer of clang 15 and older lowers it to a plain
// `memcpy` call, which link time optimization (`-prod`) removes when the copy is unused.
fn test_race_blank_reads_of_struct_values_read_their_storage() ! {
	$if !(linux || macos || freebsd || netbsd) {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'race_blank_read_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'struct Inner {
	x int
}
struct Cell {
mut:
	s  string
	n  int
	in Inner
}
struct Counter {
mut:
	value int
}
fn returned_cell() Cell { return Cell{s: "temporary"} }
fn read_string(c &Cell) { _ = c.s }
fn read_struct(c &Cell) { _ = c.in }
fn read_paren(c &Cell) { _ = (c.s) }
fn read_mut_param(mut c Cell) { _ = c }
fn next_index(mut calls Counter) int { calls.value++; return 0 }
fn read_indexed(cells []Cell, mut calls Counter) { _ = cells[next_index(mut calls)] }
fn read_scalar(c &Cell) { _ = c.n }
fn read_temporary() { _ = returned_cell() }
fn read_temporary_selector() { _ = returned_cell().s }
fn main() {
	mut cell := Cell{s: "hello", n: 1, in: Inner{x: 2}}
	read_string(&cell)
	read_struct(&cell)
	read_paren(&cell)
	read_mut_param(mut cell)
	mut calls := Counter{}
	read_indexed([cell], mut calls)
	assert calls.value == 1
	read_scalar(&cell)
	read_temporary()
	read_temporary_selector()
	assert cell.s == "hello"
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
		for name in ['read_string', 'read_struct', 'read_paren', 'read_mut_param', 'read_indexed'] {
			body := race_blank_read_function_body(output, name)
			assert body.contains('__tsan_read_range((void*)__race_blank_read') == race, '${name}: ${body}'
			assert !body.contains('volatile'), '${name}: ${body}'
		}
		if race {
			assert race_blank_read_function_body(output, 'read_string').contains('&((c->s))')
			assert race_blank_read_function_body(output, 'read_mut_param').contains('&(*(c))')
		}
		indexed := race_blank_read_function_body(output, 'read_indexed')
		assert indexed.count('next_index(') == 1, indexed
		// Scalars are loaded by an instrumented load, and calls produce C rvalues that have
		// no address: both keep the volatile copy.
		for name in ['read_scalar', 'read_temporary', 'read_temporary_selector'] {
			body := race_blank_read_function_body(output, name)
			assert !body.contains('__tsan_read_range('), '${name}: ${body}'
			assert body.contains('volatile __auto_type __race_blank_read') == race, '${name}: ${body}'
		}
		checked := cmdexec.run('cc', ['-std=gnu11', '-w', '-fsyntax-only', c_file])
		assert checked.exit_code == 0, checked.output
	}
	// The single evaluation of the index is checked by running the program, which does not
	// need a working ThreadSanitizer runtime on the test host.
	ran := cmdexec.run(@VEXE, ['-nocache', '-cc', 'cc', '-no-retry-compilation', 'run', source])
	assert ran.exit_code == 0, ran.output
}

fn race_blank_read_function_body(source string, name string) string {
	lines := source.split_into_lines()
	for i, line in lines {
		if line.ends_with(' {') && line.all_before('(').all_after_last(' ').ends_with(name) {
			return lines[i + 1..].join('\n').all_before('\n}')
		}
	}
	panic('missing generated function ${name}')
}
