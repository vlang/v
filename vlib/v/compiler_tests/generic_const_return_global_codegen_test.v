import os
import v.tempname

fn test_inferred_generic_const_return_has_concrete_global_type() {
	dir := os.join_path(os.vtmp_dir(), 'v3_generic_const_global_${tempname.unique_token()}')
	os.mkdir(dir)!
	defer { os.rmdir_all(dir) or {} }
	source := os.join_path(dir, 'v3_generic_const_global_input.v')
	os.write_file(source,
		"import sync.stdatomic\n\nconst flag = stdatomic.new_atomic(false)\nconst number = stdatomic.new_atomic(7)\n\nfn main() {\n\tassert !flag.load()\n\tassert number.load() == 7\n\tprintln('ok')\n}\n")!
	out := os.exec([@VEXE, '-no-parallel', 'run', source])
	assert out.exit_code == 0, out.output
	// `run` prints no C compiler lines before the program output any more.
	assert out.output.split_into_lines().any(it == 'ok'), out.output
}
