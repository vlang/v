import os

// An option or result type that only `sizeof` names still needs its C typedef.
fn test_sizeof_names_option_and_result_types() {
	dir := os.join_path(os.vtmp_dir(), 'sizeof_option_result_type_${os.getpid()}')
	os.mkdir_all(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	source := os.join_path(dir, 'main.v')
	os.write_file(source, 'struct Pair {
	head u8
	text string
}

fn main() {
	println(sizeof(?u64) > sizeof(u64))
	println(sizeof(!string) > sizeof(string))
	println(sizeof(![2]Pair) > sizeof([2]Pair))
}
')!
	res := os.exec([@VEXE, 'run', source])
	assert res.exit_code == 0, res.output
	assert res.output.trim_space() == 'true\ntrue\ntrue'
}
