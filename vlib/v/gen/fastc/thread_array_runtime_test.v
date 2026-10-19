module fastc

import os
import v.cmdexec

fn test_fastc_spawned_thread_arrays_keep_handles_and_typed_results() {
	$if !macos && !linux {
		return
	}
	vroot := os.real_path(os.join_path(@DIR, '../../../..'))
	root := os.join_path(os.vtmp_dir(), 'fastc_thread_arrays_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main
struct Item { value int }
fn integer(value int) int { return value }
fn item(value int) Item { return Item{value: value} }
fn mark(value int, mut values []int) { values[value] = value + 1 }
fn main() {
	large := int(1) << 40
	mut integers := []thread int{}
	mut items := []thread Item{}
	mut completed := []thread{}
	mut marks := []int{len: 8}
	for i in 0 .. 8 {
		integers << spawn integer(large + i + 100)
		items << spawn item(i + 200)
		completed << spawn mark(i, mut marks)
	}
	values := integers.wait()
	records := items.wait()
	completed.wait()
	for i in 0 .. 8 {
		assert values[i] == large + i + 100
		assert records[i].value == i + 200
		assert marks[i] == i + 1
	}
	mut direct := []thread{}
	direct << spawn mark(0, mut marks)
	for handle in direct { handle.wait() }
	wide := spawn integer(large)
	assert wide.wait() == large
	println("thread arrays work")
}
') or { panic(err) }
	program := os.join_path(root, 'program')
	build := cmdexec.run_in_merged_with_env(@VEXE, ['-b', 'fastc', '-nocache', '-gc', 'none', '-o',
		program, root], vroot, {
		'VEXE':                   @VEXE
		'VFLAGS':                 ''
		'V_MACOS_V3_NO_FALLBACK': '1'
	})
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'thread arrays work', run.output
}
