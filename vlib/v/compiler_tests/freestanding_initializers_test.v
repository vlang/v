import os

fn test_freestanding_c_initializers_follow_tls_and_finalizers_reverse_order() ! {
	$if !linux || (!amd64 && !arm64) {
		return
	}
	previous := os.getenv_opt('V_FREESTANDING_INIT_TEST')
	os.setenv('V_FREESTANDING_INIT_TEST', 'ready-λ', true)
	defer {
		if value := previous {
			os.setenv('V_FREESTANDING_INIT_TEST', value, true)
		} else {
			os.unsetenv('V_FREESTANDING_INIT_TEST')
		}
	}
	work := os.join_path(os.vtmp_dir(), 'freestanding_initializers_${os.getpid()}')
	os.mkdir_all(work)!
	defer { os.rmdir_all(work) or {} }
	source := os.join_path(@VEXEROOT, 'vlib', 'builtin', 'testdata',
		'freestanding_initializers', 'main.c.v')
	for compiler in ['clang', 'gcc'] {
		if _ := os.find_abs_path_of_executable(compiler) {} else { continue }
		for role in ['plain', 'vlibc', 'prod'] {
			program := os.join_path(work, '${compiler}-${role}')
			mut command := [@VEXE, '-no-retry-compilation', '-cc', compiler, '-freestanding', '-o',
				program]
			if role != 'plain' { command << ['-d', 'vlibc'] }
			if role == 'prod' { command << '-prod' }
			command << source
			built := os.exec(command)
			assert built.exit_code == 0, built.output
			ran := os.exec([program, 'alpha beta', 'λ'])
			assert ran.exit_code == 0, ran.output
			assert ran.output == 'PABMZY\n', ran.output
		}
	}
}
