module fastc

import os
import v.cmdexec

fn test_parallel_option_fields_keep_payloads_for_guards_and_fallbacks() ! {
	root := os.join_path(os.vtmp_dir(), 'fastc_parallel_option_fields_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main
struct Insets {
    left ?f64
    right ?f64
}
fn adjust(insets Insets) Insets {
    mut left, mut right := insets.left, insets.right
    if value := right { right = value - 2.5 }
    if value := left { left = value + 1.25 }
    return Insets{left: left, right: right}
}
fn main() {
    both := adjust(Insets{left: 1.5, right: 7.75})
    assert (both.left or { -1 }) == 2.75
    assert (both.right or { -1 }) == 5.25
    one := adjust(Insets{left: 8.5})
    assert (one.left or { -1 }) == 9.75
    if _ := one.right { panic("unexpected right") }
    empty := adjust(Insets{})
    if _ := empty.left { panic("unexpected left") }
    mut maybe, count := both.left, 42
    if value := maybe { maybe = value + 0.5 }
    assert (maybe or { -1 }) == 3.25
    assert count == 42
    println("parallel option fields work")
}
')!
	for backend in ['c', 'fastc'] {
		program := os.join_path(root, backend)
		build := cmdexec.run(@VEXE, ['-b', backend, '-o', program, root])
		assert build.exit_code == 0, build.output
		run := cmdexec.run(program, [])
		assert run.exit_code == 0, run.output
		assert run.output.trim_space() == 'parallel option fields work', run.output
	}
}
