module fastc

import os
import v.cmdexec

fn test_callback_aliases_do_not_retype_option_values_in_other_functions() ! {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_callback_signature_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main
struct Value { items []int }
fn increment(value int) int { return value + 1 }
fn first() {
    callback := increment
    assert callback(2) == 3
}
fn source() ?Value { return Value{items: [4, 2]} }
fn second() int {
    maybe := source()
    value := maybe or { return 0 }
    callback := fn [value] () int {
        mut total := 0
        for item in value.items { total += item }
        return total
    }
    return callback()
}
fn third() int {
    for callback in [increment] { assert callback(6) == 7 }
    maybe := source()
    value := maybe or { return 0 }
    callback := fn [value] () int { return value.items[1] }
    return callback()
}
fn main() {
    first()
    assert second() == 6
    assert third() == 2
}
')!
	for backend in ['c', 'fastc'] {
		program := os.join_path(root, 'program_' + backend)
		build := cmdexec.run(@VEXE, ['-b', backend, '-nocache', '-o', program, root])
		assert build.exit_code == 0, '${backend}: ${build.output}'
		run := cmdexec.run(program, [])
		assert run.exit_code == 0, '${backend}: ${run.output}'
	}
}
