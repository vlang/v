module fastc

import os
import v.cmdexec

fn test_leading_mut_applies_to_every_parallel_declaration_target() ! {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_parallel_declaration_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main
fn pair() (int, int) { return 3, 4 }
fn missing() ?(int, int) { return none }
fn main() {
    mut left, top, right := 1, 2, 3
    left, top, right = top, right, left
    assert left == 2 && top == 3 && right == 1
    mut a, b := pair()
    a, b = b, a
    assert a == 4 && b == 3
    mut x, y := missing() or { 5, 6 }
    x, y = y, x
    assert x == 6 && y == 5
    first, mut second := 7, 8
    second = first
    assert second == 7
}
')!
	for backend in ['c', 'fastc'] {
		program := os.join_path(root, 'program_' + backend)
		build := cmdexec.run(@VEXE, ['-b', backend, '-nocache', '-o', program, root])
		assert build.exit_code == 0, '${backend}: ${build.output}'
		run := cmdexec.run(program, [])
		assert run.exit_code == 0, '${backend}: ${run.output}'
	}
	// A later per-name modifier does not make subsequent targets mutable.
	os.write_file(os.join_path(root, 'main.v'), r'module main
fn main() {
    first, mut second, third := 1, 2, 3
    second, third = first, second
}
')!
	for backend in ['c', 'fastc'] {
		build := cmdexec.run(@VEXE, ['-b', backend, '-nocache', '-o', os.join_path(root, 'invalid'),
			root])
		assert build.exit_code != 0, '${backend}: accepted an immutable assignment'
		assert build.output.contains('third') && build.output.contains('immutable'), '${backend}: ${build.output}'
	}
}
