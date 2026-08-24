import os

fn test_preinclude_uncertain_macro_keeps_c_extern_prototype() {
	$if macos || linux {
		root := os.join_path(os.vtmp_dir(), 'v3_preinclude_uncertain_macro_${os.getpid()}')
		os.rmdir_all(root) or {}
		os.mkdir_all(root)!
		defer {
			os.rmdir_all(root) or {}
		}
		header := os.join_path(root, 'api.h')
		implementation := os.join_path(root, 'api.c')
		source := os.join_path(root, 'main.c.v')
		output := os.join_path(root, 'main')
		os.write_file(header, '#ifdef __MSVC_ONLY__\n#define compiler_api(x) ((x) + 1)\n#endif\n')!
		os.write_file(implementation, 'int compiler_api(int x) { return x + 1; }\n')!
		os.write_file(source, 'module main

#preinclude "${header}"
#flag ${implementation}

fn C.compiler_api(int) int

fn main() {
	assert C.compiler_api(41) == 42
}
')!
		mut environment := os.environ()
		environment['VFLAGS'] = ''
		environment['VOSARGS'] = ''
		mut compiler := os.new_process(@VEXE)
		compiler.set_args(['-new-compiler', '-gc', 'none', '-o', output, source])
		compiler.set_environment(environment)
		compiler.set_redirect_stdio()
		compiler.run()
		compiler.wait()
		compiler_output := compiler.stdout_slurp() + compiler.stderr_slurp()
		compiler_exit_code := compiler.code
		compiler.close()
		assert compiler_exit_code == 0, compiler_output
		run := os.execute(os.quoted_path(output))
		assert run.exit_code == 0, run.output
	}
}
