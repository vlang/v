import os
import v.tempname

fn test_for_in_blank_index_uses_synthetic_c_loop_index() {
	root := os.join_path(os.vtmp_dir(), 'v3_for_blank_index_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	src := os.join_path(root, 'v3_for_blank_index_input.v')
	os.write_file(src, 'fn main() {
	arr := [1, 2, 3]
	mut sum := 0
	for _, value in arr {
		sum += value
	}
	assert sum == 6
	println("ok")
}
') or {
		panic(err)
	}
	bin := os.join_path(root, 'v3_for_blank_index_input')
	compile := os.exec([@VEXE, '${src}', '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	c_code := os.read_file(bin + '.c') or { panic(err) }
	assert !c_code.contains('for (; _ < arr.len; _++)'), c_code
	assert !c_code.contains('array_get(arr, _)'), c_code
	assert c_code.contains('__for_idx_') || c_code.contains('__discard_'), c_code

	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'
}
