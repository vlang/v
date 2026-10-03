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
		result := os.exec([@VEXE, '-new-compiler', ...(os.split_args(flags) or { panic(err) }),
			'-o', output, source])
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
	mut executable := os.join_path(root, 'program')
	$if windows {
		executable += '.exe'
	}
	result := os.exec([@VEXE, '-new-compiler', '-cg', '-gc', 'none', '-nocache', '-o', executable,
		source])
	assert result.exit_code == 0, result.output
	// The driver names the retained directory after the final executable file.
	dir_prefix := '.${os.base(executable)}.v3cc.'
	dirs := os.ls(root)!.filter(it.starts_with(dir_prefix))
	assert dirs.len == 1, dirs.str()
	generated := os.read_file(os.join_path(root, dirs[0], 'src.c'))!
	assert generated.contains('debug source')
	assert !generated.contains('#line ')
	run := os.exec([executable])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'debug source'
}

fn test_macos_clang_debug_symbols_follow_the_final_binary() {
	$if !macos {
		return
	}
	clang := os.find_abs_path_of_executable('clang') or { return }
	dwarfdump := os.find_abs_path_of_executable('dwarfdump') or { return }
	_ := os.find_abs_path_of_executable('dsymutil') or { return }
	root := os.join_path(os.vtmp_dir(), 'debug_symbols_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'debug_position.v')
	os.write_file(source, '@[noinline]
fn debug_position() int {
 return 42
}
fn main() {
 println(debug_position())
}
')!
	for flags in ['-g', '-cg'] {
		executable := os.join_path(root, 'program_${flags.all_after('-')}')
		build := os.exec([@VEXE, '-new-compiler', ...(os.split_args(flags) or { panic(err) }),
			'-gc', 'none', '-nocache', '-cc', '${clang}', '-o', executable, source])
		assert build.exit_code == 0, build.output
		bundle := executable + '.dSYM'
		assert os.is_dir(bundle), 'missing debug symbols beside ${executable}'
		symbols := os.exec(['${dwarfdump}', '--debug-info', '--name=debug_position', '${bundle}'])
		assert symbols.exit_code == 0, symbols.output
		if flags == '-g' {
			assert symbols.output.contains(os.real_path(source)), symbols.output
		} else {
			assert symbols.output.contains('src.c'), symbols.output
			assert !symbols.output.contains('debug_position.v'), symbols.output
		}
	}
}
