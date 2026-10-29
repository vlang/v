import os
import strings
import v.cmdexec

fn test_tcc_macos_preserves_zero_initialized_globals() {
	tcc_dir := os.join_path(@VEXEROOT, 'thirdparty', 'tcc')
	tcc := os.join_path(tcc_dir, 'tcc.exe')
	if !os.is_file(tcc) {
		return
	}
	dir := os.join_path(os.vtmp_dir(), 'tcc_zero_globals_${os.getpid()}')
	os.mkdir_all(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	mut source := strings.new_builder(400000)
	source.write_string('#include <stdio.h>\nchar data_bytes[100] = {1};\nchar *data_ptr = data_bytes;\nstatic char zero_bytes[32768];\n')
	// A large symbol table keeps the file long enough for dyld page-in linking.
	// Chained fixups must exclude the zero-fill pages even when they fit in the file.
	for i in 0 .. 6000 {
		source.writeln('int padding_function_with_a_long_name_${i}(void) { return ${i}; }')
	}
	source.write_string('int main(void) {
	long bad = 0;
	for (long i = 0; i < (long)sizeof(zero_bytes); i++) bad += zero_bytes[i] != 0;
	printf("non-zero __bss bytes: %ld\\n", bad);
	return bad != 0 || data_ptr != data_bytes || data_ptr[0] != 1;
}
')
	cfile := os.join_path(dir, 'globals.c')
	executable := os.join_path(dir, 'globals')
	os.write_file(cfile, source.str())!
	compile := cmdexec.run(tcc, ['-B', tcc_dir, '-o', executable, cfile])
	assert compile.exit_code == 0, compile.output
	run := cmdexec.run(executable, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'non-zero __bss bytes: 0', run.output
}
