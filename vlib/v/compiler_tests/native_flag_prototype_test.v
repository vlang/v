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
	native_dir := os.join_path(root, 'native sources')
	os.mkdir_all(native_dir)!
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(native_dir, 'quoted.c'),
		'int native_quoted_value(void) { return 73; }\n')!
	os.write_file(os.join_path(native_dir, 'first_existing.c'),
		'int native_first_existing_value(void) { return 74; }\n')!
	os.write_file(os.join_path(native_dir, 'when_first_existing.c'),
		'int native_when_first_existing_value(void) { return 75; }\n')!
	output := compile_and_run_native_flag_program(root, 'resolved', '#flag "@DIR/native sources/quoted.c"
#flag \$first_existing("@DIR/missing.c", "@DIR/native sources/first_existing.c")
#flag \$when_first_existing("@DIR/missing.c", "@DIR/native sources/when_first_existing.c")

fn C.native_quoted_value() i32
fn C.native_first_existing_value() i32
fn C.native_when_first_existing_value() i32

fn main() {
	assert C.native_quoted_value() == 73
	assert C.native_first_existing_value() == 74
	assert C.native_when_first_existing_value() == 75
}
')
	generated := os.read_file(output + '.c')!
	for name in ['quoted', 'first_existing', 'when_first_existing'] {
		assert generated.contains('i32 native_${name}_value(void);'), name
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
	compile_and_run_native_flag_program(root, name, '#flag ${flag}\nfn C.native_flag_value() i32\n' +
		'fn main() { assert C.native_flag_value() == 73 }\n')
}

fn compile_and_run_native_flag_program(root string, name string, program string) string {
	source := os.join_path(root, '${name}.v')
	output := os.join_path(root, name)
	os.write_file(source, program) or { panic(err) }
	compiled := os.exec([@VEXE, '-b', 'c', '-o', output, source])
	assert compiled.exit_code == 0, compiled.output
	result := os.exec([output])
	assert result.exit_code == 0, result.output
	return output
}
