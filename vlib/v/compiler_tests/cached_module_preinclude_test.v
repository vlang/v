import os

const vexe = @VEXE

// A cached module is read back from its declaration header. A `#preinclude`
// missing from that header changes the C flags the module's object was built
// with, so the object is rebuilt on every build instead of being reused.
fn test_a_cached_module_keeps_its_preinclude() {
	root := os.join_path(os.vtmp_dir(), 'cached_module_preinclude_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'vmods', 'preincluded'))!
	os.mkdir_all(os.join_path(root, 'app'))!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'vmods', 'preincluded', 'preincluded.v'),
		'module preincluded\n\n#preinclude <iso646.h>\n\npub fn value() int {\n\treturn 7\n}\n')!
	os.write_file(os.join_path(root, 'app', 'main.v'),
		'import preincluded\n\nfn main() {\n\tprintln(preincluded.value())\n}\n')!
	program_c := os.join_path(root, 'program.c')
	output := os.join_path(root, 'program')
	os.setenv('VMODULES', os.join_path(root, 'vmods'), true)
	os.setenv('V3_DISABLE_UNCHANGED_BUILD_CACHE', '1', true)
	os.setenv('V3_CACHE_KEEP_PROGRAM_C', program_c, true)
	os.setenv('V3_CACHE_TRACE', '1', true)
	defer {
		os.unsetenv('VMODULES')
		os.unsetenv('V3_DISABLE_UNCHANGED_BUILD_CACHE')
		os.unsetenv('V3_CACHE_KEEP_PROGRAM_C')
		os.unsetenv('V3_CACHE_TRACE')
	}
	mut last_output := ''
	for _ in 0 .. 3 {
		built := os.exec([vexe, '-o', output, os.join_path(root, 'app', 'main.v')])
		assert built.exit_code == 0, built.output
		last_output = built.output
	}
	assert !last_output.contains('cache object miss: module=preincluded'), last_output
	assert os.read_file(program_c)!.contains('#include <iso646.h>')
	assert os.exec([output]).output.trim_space() == '7'
}
