module fastc

import os
import v.cmdexec

fn test_fixed_array_closure_captures_keep_value_and_reference_semantics() ! {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_fixed_array_capture_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main
fn snapshot(value [4]f32) fn () f32 {
    return fn [value] () f32 { return value[0] + value[3] }
}
fn main() {
    mut values := [f32(1), 2, 3, 4]!
    saved := snapshot(values)
    values[0] = 100
    assert saved() == 5
    increment := fn [mut values] () f32 { values[1] += 1; return values[1] }
    assert increment() == 3
    assert increment() == 4
    assert values[1] == 2
    matrix := [[1, 2]!, [3, 4]!]!
    nested := fn [matrix] () int {
        inner := fn [matrix] () int { return matrix[1][0] }
        return inner()
    }
    assert nested() == 3
}
')!
	program := os.join_path(root, 'program')
	build := cmdexec.run(@VEXE, ['-b', 'fastc', '-nocache', '-o', program, root])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
}
