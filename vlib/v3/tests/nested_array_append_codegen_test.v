import os

const nested_array_append_vexe = @VEXE
const nested_array_append_tests_dir = os.dir(@FILE)
const nested_array_append_v3_dir = os.dir(nested_array_append_tests_dir)
const nested_array_append_vlib_dir = os.dir(nested_array_append_v3_dir)
const nested_array_append_v3_src = os.join_path(nested_array_append_v3_dir, 'v3.v')

fn nested_array_append_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_nested_array_append_test_${os.getpid()}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${nested_array_append_vexe} -gc none -path "${nested_array_append_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${nested_array_append_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn test_nested_array_append_keeps_push_and_push_many_semantics() {
	v3_bin := nested_array_append_build_v3()
	src := os.join_path(os.temp_dir(), 'v3_nested_array_append_input_${os.getpid()}.v')
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

	bin := os.join_path(os.temp_dir(), 'v3_nested_array_append_input_${os.getpid()}')
	os.rm(bin) or {}
	os.rm(bin + '.c') or {}
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	run := os.execute(bin)
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
