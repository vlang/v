import os

const mutable_index_move_vexe = @VEXE
const mutable_index_move_tests_dir = os.dir(@FILE)
const mutable_index_move_v3_dir = os.dir(mutable_index_move_tests_dir)
const mutable_index_move_vlib_dir = os.dir(mutable_index_move_v3_dir)
const mutable_index_move_v3_src = os.join_path(mutable_index_move_v3_dir, 'v3.v')

fn test_mutable_array_field_index_move_clears_source_slot() {
	pid := os.getpid()
	v3_bin := os.join_path(os.temp_dir(), 'v3_mutable_index_move_${pid}')
	source := os.join_path(os.temp_dir(), 'v3_mutable_index_move_input_${pid}.v')
	output := os.join_path(os.temp_dir(), 'v3_mutable_index_move_program_${pid}')
	defer {
		os.rm(v3_bin) or {}
		os.rm(source) or {}
		os.rm(output) or {}
		os.rm(output + '.c') or {}
	}
	build :=
		os.execute('${mutable_index_move_vexe} -gc none -d ownership -path "${mutable_index_move_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${mutable_index_move_v3_src}')
	assert build.exit_code == 0, build.output
	os.write_file(source, "struct Item {
\tpath string
}

struct Queue {
mut:
\titems []Item
\tnext int
}

fn (mut queue Queue) pop() Item {
\titem := queue.items[queue.next]
\tqueue.next++
\treturn item
}

fn main() {
\tmut queue := Queue{
\t\titems: [Item{
\t\t\tpath: 'owned path'.to_owned()
\t\t}]
\t}
\titem := queue.pop()
\tassert item.path == 'owned path'
\tassert queue.items[0].path == ''
\tprintln('ok')
}
") or {
		panic(err)
	}
	compile :=
		os.execute('${v3_bin} -ownership -d ownership -nocache -no-parallel -o ${output} ${source}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(output)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok', run.output
}
