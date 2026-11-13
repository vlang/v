module fastc

import os
import v.cmdexec

fn test_fastc_integer_closure_capture_source_types() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_integer_capture_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main

fn increment(value uint) uint {
	return value + 1
}

fn main() {
	value := uint(7)
	scalar := fn [value] () uint { return value }
	assert scalar() == uint(7)

	values := [uint(9), 10]
	array_capture := fn [values] () uint { return values[0] + values[1] }
	assert array_capture() == uint(19)

	entries := {"value": uint(11)}
	map_capture := fn [entries] () uint { return entries["value"] }
	assert map_capture() == uint(11)

	mut pointed := uint(13)
	pointer := &pointed
	pointer_capture := fn [pointer] () uint { return *pointer }
	assert pointer_capture() == uint(13)
	pointed = uint(14)
	assert pointer_capture() == uint(14)

	callback := increment
	callback_capture := fn [callback] (argument uint) uint { return callback(argument) }
	assert callback_capture(uint(15)) == uint(16)
}
') or { panic(err) }
	program := os.join_path(root, 'program')
	build := cmdexec.run(@VEXE, ['-b', 'fastc', '-gc', 'none', '-nocache', '-o', program, root])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
}

fn test_fastc_nested_offsetof_preserves_integer_field_names() {
	root := os.join_path(os.vtmp_dir(), 'fastc_nested_integer_offsetof_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main

struct Layout {
	prefix u8
	usize u64
	isize u32
}

struct Control {
	prefix u8
	first u64
	second u32
}

fn identity(value int) int { return value }
fn equal(left int, right int) bool { return left == right }

fn main() {
	usize := 31
	isize := -17
	assert equal(identity(int(__offsetof(Layout, usize))), int(__offsetof(Control, first)))
	assert equal(identity(int(__offsetof(Layout, isize))), int(__offsetof(Control, second)))
	offsets := [int(__offsetof(Layout, usize)), int(__offsetof(Layout, isize))]
	assert offsets[0] == int(__offsetof(Control, first))
	assert offsets[1] == int(__offsetof(Control, second))
	assert equal(usize, 31)
	assert equal(isize, -17)
}
') or { panic(err) }
	program := os.join_path(root, 'program')
	build := cmdexec.run(@VEXE, ['-b', 'fastc', '-gc', 'none', '-nocache', '-o', program, root])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
}
