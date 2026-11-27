import os

fn test_linker_inputs_keep_c_function_prototypes() {
	root := os.join_path(os.vtmp_dir(), 'linker_object_flag_prototypes_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'helper.c')
	object := os.join_path(root, 'helper.o')
	os.write_file(source, 'int native_object_value(void) { return 73; }\n')!
	compiled := os.exec(['cc', '-c', source, '-o', object])
	assert compiled.exit_code == 0, compiled.output

	$if macos {
		// Keep the archive's module-level linkage away from the object binding.
		archive_root := os.join_path(root, 'archive_case')
		os.mkdir(archive_root)!
		member_source := os.join_path(archive_root, 'member.c')
		member_object := os.join_path(archive_root, 'member.o')
		archive := os.join_path(archive_root, 'helper.a')
		os.write_file(member_source, 'int native_archive_value(void) { return 74; }\n')!
		member_build := os.exec(['cc', '-c', member_source, '-o', member_object])
		assert member_build.exit_code == 0, member_build.output
		archived := os.exec(['ar', 'rcs', archive, member_object])
		assert archived.exit_code == 0, archived.output
		os.write_file(os.join_path(archive_root, 'archive_case.v'), 'module archive_case

#flag -Wl,-force_load,@DIR/helper.a
fn C.native_archive_value() i32

pub fn value() i32 {
	return C.native_archive_value()
}
')!
	}

	output := compile_and_run_native_flag_program(root, 'linker_inputs', 'module main

\$if macos {
	import archive_case
}

#flag -Wl,@DIR/helper.o
fn C.native_object_value() i32

fn main() {
	assert C.native_object_value() == 73
	\$if macos {
		assert archive_case.value() == 74
	}
}
')
	generated := os.read_file(output + '.c')!
	assert generated.contains('i32 native_object_value(void);')
	$if macos {
		assert generated.contains('i32 native_archive_value(void);')
	}
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
	os.write_file(os.join_path(root, 'separate'),
		'int native_separate_value(void) { return 73; }\n')!
	os.write_file(os.join_path(root, 'joined'), 'int native_joined_value(void) { return 74; }\n')!
	output := compile_and_run_native_flag_program(root, 'forced_c', '#flag -x c @DIR/separate -x none
#flag -xc @DIR/joined -xnone

fn C.native_separate_value() i32
fn C.native_joined_value() i32

fn main() {
	assert C.native_separate_value() == 73
	assert C.native_joined_value() == 74
}
')
	generated := os.read_file(output + '.c')!
	assert generated.contains('i32 native_separate_value(void);')
	assert generated.contains('i32 native_joined_value(void);')
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
