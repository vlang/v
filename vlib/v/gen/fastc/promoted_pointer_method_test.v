module fastc

import os
import v.cmdexec

fn test_promoted_pointer_methods_in_compound_expressions_match_c() ! {
	root := os.join_path(os.vtmp_dir(), 'fastc_promoted_pointer_method_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main
struct Record {
    value u64
}
fn (record &Record) value() u64 { return record.value }
struct State {
    record &Record
}
type StateRef = &State
struct App {
    StateRef
}
fn score(app &App) u64 {
    mut result := u64(1)
    result += app.record.value() * 31 + app.record.value() * 17
    return result
}
fn main() {
    record := Record{value: 7}
    state := State{record: &record}
    app := App{StateRef: &state}
    assert score(&app) == 337
    assert app.record.value() + app.record.value() == 14
    assert 3 * app.record.value() == 21
    println("promoted pointer methods work")
}
')!
	for backend in ['c', 'fastc'] {
		program := os.join_path(root, backend)
		build := cmdexec.run(@VEXE, ['-b', backend, '-o', program, root])
		assert build.exit_code == 0, build.output
		run := cmdexec.run(program, [])
		assert run.exit_code == 0, run.output
		assert run.output.trim_space() == 'promoted pointer methods work', run.output
	}
}
