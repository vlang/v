import os

fn test_resolved_native_sources_keep_c_function_prototypes() {
	root := os.join_path(os.vtmp_dir(), 'native_flag_prototypes_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'native sources'))!
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'native sources', 'helper.c'),
		'int native_flag_value(void) { return 73; }\n')!
	for i, flag in ['"@DIR/native sources/helper.c"',
		'\$first_existing("@DIR/missing.c", "@DIR/native sources/helper.c")',
		'\$when_first_existing("@DIR/missing.c", "@DIR/native sources/helper.c")'] {
		compile_and_run_native_flag(root, 'resolved_${i}', flag)
	}
}

fn test_objective_c_sources_keep_c_function_prototypes() {
	$if macos {
		root := os.join_path(os.vtmp_dir(), 'objective_c_flag_prototypes_${os.getpid()}')
		os.mkdir_all(root)!
		defer {
			os.rmdir_all(root) or {}
		}
		os.write_file(os.join_path(root, 'helper.m'),
			'int native_flag_value(void) { return 73; }\n')!
		compile_and_run_native_flag(root, 'objective_c', '@DIR/helper.m')
	}
}

fn compile_and_run_native_flag(root string, name string, flag string) {
	source := os.join_path(root, '${name}.v')
	output := os.join_path(root, name)
	os.write_file(source, '#flag ${flag}\nfn C.native_flag_value() i32\n' +
		'fn main() { assert C.native_flag_value() == 73 }\n') or { panic(err) }
	compiled := os.exec([@VEXE, '-b', 'c', '-o', output, source])
	assert compiled.exit_code == 0, compiled.output
	result := os.exec([output])
	assert result.exit_code == 0, result.output
}
