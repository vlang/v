module fastc

import os
import v.cmdexec
import v.pref

const nested_index_mutation_source = r'module main

fn increment(mut count int) {
	count++
}

fn decrement(mut count int) {
	count--
}

fn counted_index(mut calls int, value int) int {
	calls++
	return value
}

fn main() {
	mut count := 10
	increment(mut count)
	assert count == 11
	decrement(mut count)
	assert count == 10
	mut groups := [][]int{}
	groups << [1, 2]
	groups << [3, 4]
	groups[groups.len - 1][0]++
	groups.last()[1]--
	assert groups[1][0] == 4
	assert groups[1][1] == 3
	mut calls := 0
	groups[counted_index(mut calls, 0)][counted_index(mut calls, 1)]++
	assert calls == 2
	assert groups[0][1] == 3
	mut value := 42
	mut pointer := &value
	unsafe {
		pointer++
		pointer--
	}
	assert unsafe { *pointer } == 42
	println("indexed mutation")
}
'

fn test_fastc_nested_index_mutations_preserve_storage_and_evaluate_indices_once() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate(nested_index_mutation_source, 'nested_index_mutation.v', prefs) or {
		panic(err)
	}
	assert c_source.contains('(*('), c_source
	root := os.join_path(os.vtmp_dir(), 'fastc_index_mutation_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), nested_index_mutation_source) or { panic(err) }
	program := os.join_path(root, 'program')
	compiled := cmdexec.run(@VEXE, ['-b', 'fastc', '-gc', 'none', '-no-prealloc', '-nocache', '-o',
		program, root])
	assert compiled.exit_code == 0, compiled.output
	executed := cmdexec.run(program, [])
	assert executed.exit_code == 0, executed.output
	assert executed.output == 'indexed mutation\n'
}
