import os
import time
import v.cmdexec

fn test_header_alias_cycles_are_left_to_the_c_preprocessor() ! {
	root := os.join_path(os.vtmp_dir(), 'native_header_policy_${os.getpid()}_${time.now().unix_nano()}')
	os.mkdir_all(os.join_path(root, 'sub'))!
	defer { os.rmdir_all(root) or {} }
	header := os.join_path(root, 'native.h')
	inner := os.join_path(root, 'inner.h')
	mut recursive_include := '#include "sub/../native.h"\n'
	$if !windows {
		os.symlink(root, os.join_path(root, 'alias'))!
		recursive_include += '#include "alias/native.h"\n'
	}
	os.write_file(header, '#ifndef NATIVE_HEADER_POLICY_H\n#define NATIVE_HEADER_POLICY_H\n${recursive_include}#include "inner.h"\nstatic int header_policy_answer(void) { return HEADER_POLICY_SENTINEL; }\n#endif\n')!
	source := os.join_path(root, 'main.c.v')
	os.write_file(source, 'module main\n#include "@DIR/native.h"\nfn C.header_policy_answer() int\nfn main() { println(C.header_policy_answer()) }\n')!
	c_output := os.join_path(root, 'main.c')
	for cross in [false, true] {
		mut args := ['-new-compiler', '-gc', 'none', '-o', c_output]
		if cross { args << '-cross' }
		args << source
		generated := cmdexec.run(@VEXE, args)
		assert generated.exit_code == 0, generated.output
		text := os.read_file(c_output)!
		assert text.contains('#include "${header.replace('\\', '/')}"')
			|| text.contains('#include "${os.real_path(header).replace('\\', '/')}"')
		assert !text.contains('HEADER_POLICY_SENTINEL')
		assert !text.contains(recursive_include)
	}
	// C include guards resolve both aliases. Editing a nested header must still
	// affect a subsequent native build, including a requested parallel build.
	binary := os.join_path(root, 'program')
	for value in [41, 42] {
		os.write_file(inner, '#define HEADER_POLICY_SENTINEL ${value}\n')!
		built := cmdexec.run(@VEXE, ['-new-compiler', '-gc', 'none', '-cc', 'cc', '-parallel-cc',
			'-o', binary, source])
		assert built.exit_code == 0, built.output
		run := cmdexec.run(binary, []string{})
		assert run.exit_code == 0, run.output
		assert run.output.trim_space() == value.str()
	}
}
