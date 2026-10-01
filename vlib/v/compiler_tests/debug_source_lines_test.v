import os

fn test_debug_flags_select_v_or_c_source_positions() {
	root := os.join_path(os.vtmp_dir(), 'debug_lines_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, '@[noinline]
fn boom(x int) int {
 return 10 / x
}
fn main() {
 println(boom(2))
}
')!
	path := os.real_path(source).replace('\\', '/').replace('"', '\\"')
	for flags in ['-g', '-g -no-parallel', '-cg', ''] {
		output := os.join_path(root, 'main.c')
		result := os.execute('${os.quoted_path(@VEXE)} -new-compiler ${flags} -o ${os.quoted_path(output)} ${os.quoted_path(source)}')
		assert result.exit_code == 0, result.output
		generated := os.read_file(output)!
		if flags.starts_with('-g') {
			assert generated.contains('#line 2 "${path}"\n'), 'missing V function line directive for ${flags}'
			assert generated.contains('#line 3 "${path}"\n'), 'missing V statement line directive for ${flags}'
			assert generated.contains('#line 1 "<generated>"\n')
		} else {
			assert !generated.contains('#line ')
		}
	}
}

fn test_c_debug_build_keeps_the_source_named_by_debug_information() {
	root := os.join_path(os.vtmp_dir(), 'c_debug_source_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'fn main() { println("debug source") }')!
	executable := os.join_path(root, 'program')
	result := os.execute('${os.quoted_path(@VEXE)} -new-compiler -cg -gc none -nocache -o ${os.quoted_path(executable)} ${os.quoted_path(source)}')
	assert result.exit_code == 0, result.output
	dirs := os.ls(root)!.filter(it.starts_with('.program.v3cc.'))
	assert dirs.len == 1, dirs.str()
	generated := os.read_file(os.join_path(root, dirs[0], 'src.c'))!
	assert generated.contains('debug source')
	assert !generated.contains('#line ')
	run := os.execute(os.quoted_path(executable))
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'debug source'
}
