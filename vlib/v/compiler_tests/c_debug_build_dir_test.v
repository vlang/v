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

// The executable that is in place names its C sources in its debug information. A rebuild that
// fails in the C compiler or the linker leaves that executable where it is, so it must leave
// those sources too: the same files, not the ones of the build that failed.
fn test_a_failed_rebuild_keeps_the_sources_of_the_executable_in_place() {
	root := os.join_path(os.vtmp_dir(), 'c_debug_dir_failed_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'fn main() { println("first build") }')!
	mut executable := os.join_path(root, 'program')
	$if windows {
		executable += '.exe'
	}
	build_with_c_debug(source, executable)
	dir_prefix := '.${os.base(executable)}.v3cc.'
	first_dirs := os.ls(root)!.filter(it.starts_with(dir_prefix))
	assert first_dirs.len == 1, first_dirs.str()
	c_source := os.join_path(root, first_dirs[0], 'src.c')
	first_c := os.read_file(c_source)!
	first_executable := os.read_bytes(executable)!
	assert first_c.contains('first build')
	// The program changes, and the link cannot succeed: the library does not exist.
	os.write_file(source, 'fn main() { println("second build") }')!
	failed := os.exec([@VEXE, '-new-compiler', '-cg', '-gc', 'none', '-nocache', '-ldflags',
		'-lv3_c_debug_dir_test_no_such_library', '-o', executable, source])
	assert failed.exit_code != 0, 'the build with a missing library succeeded:\n${failed.output}'
	assert os.read_bytes(executable)! == first_executable, 'a failed build changed the executable'
	after_failure := os.ls(root)!.filter(it.starts_with(dir_prefix))
	assert after_failure == first_dirs, 'after the failed build: ${after_failure}'
	assert os.exists(c_source), 'the failed build removed ${c_source}:\n${failed.output}'
	assert os.read_file(c_source)! == first_c, 'the sources are no longer the ones of the executable in place'
	assert os.exec([executable]).output.trim_space() == 'first build'
	// A build that succeeds replaces both, and leaves one directory again.
	build_with_c_debug(source, executable)
	after_success := os.ls(root)!.filter(it.starts_with(dir_prefix))
	assert after_success == first_dirs, 'after the build that succeeded: ${after_success}'
	assert os.read_file(c_source)!.contains('second build')
	assert os.exec([executable]).output.trim_space() == 'second build'
}
