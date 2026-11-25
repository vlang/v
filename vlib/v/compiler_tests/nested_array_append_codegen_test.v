import os
import v.tempname

fn test_nested_array_append_keeps_push_and_push_many_semantics() {
	root := os.join_path(os.vtmp_dir(), 'v3_nested_array_append_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	src := os.join_path(root, 'main.v')
	os.write_file(src, "fn push_path(mut pattern [][]string, mut path []string) {
	pattern << [path]
}

fn main() {
	mut scalars := []int{}
	scalars << [1, 2]
	assert scalars.len == 2
	assert scalars[1] == 2

	mut rows := [][]int{}
	row_group := [[1], [2]]
	rows << row_group
	assert rows.len == 2
	assert rows[1][0] == 2

	mut row_items := [][]int{}
	row := [3, 4]
	row_items << row
	assert row_items.len == 1
	assert row_items[0][1] == 4

	mut cube := [][][]int{}
	cube_item := [[5], [6]]
	cube << cube_item
	assert cube.len == 1
	assert cube[0].len == 2
	assert cube[0][1][0] == 6

	mut cube_many := [][][]int{}
	cube_group := [[[7]], [[8]]]
	cube_many << cube_group
	assert cube_many.len == 2
	assert cube_many[1][0][0] == 8

	mut queue := [][][]string{}
	path := []string{}
	queue << [['A'], path]
	assert queue.len == 1
	assert queue[0].len == 2
	assert queue[0][0][0] == 'A'
	assert queue[0][1].len == 0

	mut pattern := [][]string{}
	mut dfs_path := ['A', 'B']
	push_path(mut pattern, mut dfs_path)
	assert pattern.len == 1
	assert pattern[0][1] == 'B'
	println('ok')
}
") or {
		panic(err)
	}

	bin := os.join_path(root, 'program')
	compile := os.exec([@VEXE, '${src}', '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'

	c_code := os.read_file(bin + '.c') or { panic(err) }
	assert c_code.contains('array__push_many(&scalars'), c_code
	assert c_code.contains('array__push_many(&rows'), c_code
	assert c_code.contains('array_push(&row_items'), c_code
	assert c_code.contains('array_push(&cube'), c_code
	assert c_code.contains('array__push_many(&cube_many'), c_code
	assert c_code.contains('array_push(&queue'), c_code
	assert c_code.contains('array__push_many(pattern'), c_code
}
