import os
import v.tempname

fn test_join_path_lowering_roots_join_path_single() {
	root := os.join_path(os.vtmp_dir(), 'v3_join_path_markused_input_${tempname.unique_token()}')
	os.mkdir(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	src := os.join_path(root, 'main.v')
	os.write_file(src, "import os

fn main() {
	path := os.join_path('a', 'b', 'c')
	expected := 'a' + os.path_separator + 'b' + os.path_separator + 'c'
	assert path == expected
	println(path)
}
") or {
		panic(err)
	}

	bin := os.join_path(root, 'app')
	compile := os.exec([@VEXE, src, '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	generated := os.read_file(bin + '.c') or { panic(err) }
	assert generated.contains('os__join_path_single'), generated

	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	expected := 'a' + os.path_separator + 'b' + os.path_separator + 'c'
	assert run.output.trim_space() == expected
}
