import os

// build_with_c_debug builds `source` into `executable` with `-cg` and returns the build output.
fn build_with_c_debug(source string, executable string) string {
	result := os.exec([@VEXE, '-new-compiler', '-cg', '-gc', 'none', '-nocache', '-o', executable,
		source])
	assert result.exit_code == 0, result.output
	return result.output
}

fn test_repeated_c_debug_builds_leave_one_directory_named_once() {
	root := os.join_path(os.vtmp_dir(), 'c_debug_dir_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'fn main() { println("kept once") }')!
	mut executable := os.join_path(root, 'program')
	$if windows {
		executable += '.exe'
	}
	dir_prefix := '.${os.base(executable)}.v3cc.'
	mut outputs := []string{}
	for _ in 0 .. 3 {
		outputs << build_with_c_debug(source, executable)
	}
	dirs := os.ls(root)!.filter(it.starts_with(dir_prefix))
	assert dirs.len == 1, 'three -cg builds left ${dirs.len} directories: ${dirs}'
	assert os.exists(os.join_path(root, dirs[0], 'src.c'))
	for output in outputs {
		assert output.count(dirs[0]) == 1, 'the build output names ${dirs[0]} ${output.count(dirs[0])} times:\n${output}'
	}
}

fn test_a_build_without_c_debug_names_no_directory_and_leaves_none() {
	root := os.join_path(os.vtmp_dir(), 'c_debug_dir_off_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'fn main() { println("plain") }')!
	executable := os.join_path(root, 'program')
	result := os.exec([@VEXE, '-new-compiler', '-gc', 'none', '-nocache', '-o', executable, source])
	assert result.exit_code == 0, result.output
	assert !result.output.contains('.v3cc.'), result.output
	assert os.ls(root)!.filter(it.contains('.v3cc.')).len == 0
}
