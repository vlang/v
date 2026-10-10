import os

fn test_linker_object_inputs_keep_c_function_prototypes() {
	root := os.join_path(os.vtmp_dir(), 'linker_object_flag_prototypes_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'helper.c')
	object := os.join_path(root, 'helper.o')
	os.write_file(source, 'int native_flag_value(void) { return 73; }\n')!
	compiled := os.exec(['cc', '-c', source, '-o', object])
	assert compiled.exit_code == 0, compiled.output
	compile_and_run_native_flag(root, 'linker_object', '-Wl,@DIR/helper.o')
}

fn test_portable_c_keeps_prototypes_for_other_target_native_inputs() {
	root := os.join_path(os.vtmp_dir(), 'portable_native_flag_prototypes_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	output := os.join_path(root, 'main.c')
	os.write_file(source, '#flag windows @DIR/helper.o\nfn C.portable_native_value() i32\n' +
		'fn main() { \$if windows { println(C.portable_native_value()) } }\n')!
	compiled := os.exec([@VEXE, '-os', 'cross', '-o', output, source])
	assert compiled.exit_code == 0, compiled.output
	c_source := os.read_file(output)!
	assert c_source.contains('i32 portable_native_value(void);')
}

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

fn test_forced_c_sources_keep_c_function_prototypes() {
	root := os.join_path(os.vtmp_dir(), 'forced_c_flag_prototypes_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'helper'),
		'int native_flag_value(void) { return 73; }\n')!
	compile_and_run_native_flag(root, 'forced_c', '-x c @DIR/helper -x none')
	compile_and_run_native_flag(root, 'joined_c', '-xc @DIR/helper -xnone')
}

fn test_force_loaded_archives_keep_c_function_prototypes() {
	$if macos {
		root := os.join_path(os.vtmp_dir(), 'force_load_flag_prototypes_${os.getpid()}')
		os.mkdir_all(root)!
		defer {
			os.rmdir_all(root) or {}
		}
		source := os.join_path(root, 'helper.c')
		object := os.join_path(root, 'helper.o')
		archive := os.join_path(root, 'helper.a')
		os.write_file(source, 'int native_flag_value(void) { return 73; }\n')!
		compiled := os.exec(['cc', '-c', source, '-o', object])
		assert compiled.exit_code == 0, compiled.output
		archived := os.exec(['ar', 'rcs', archive, object])
		assert archived.exit_code == 0, archived.output
		compile_and_run_native_flag(root, 'force_load', '-Wl,-force_load,@DIR/helper.a')
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
