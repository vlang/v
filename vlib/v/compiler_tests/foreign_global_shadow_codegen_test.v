import os
import v.cmdexec

fn test_foreign_global_preserves_local_and_parameter_bindings() ! {
	os.find_abs_path_of_executable('gcc') or { return }
	fixture := os.join_path(@VEXEROOT, 'vlib/v/compiler_tests/testdata/foreign_global_shadow')
	root := os.join_path(os.vtmp_dir(), 'foreign_global_shadow_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	copy := os.join_path(root, 'fixture')
	os.cp_all(fixture, copy, true)!
	old_modules := os.getenv_opt('VMODULES')
	os.setenv('VMODULES', os.join_path(copy, 'deps'), true)
	defer {
		if previous := old_modules {
			os.setenv('VMODULES', previous, true)
		} else {
			os.unsetenv('VMODULES')
		}
	}
	modes := $if linux && (amd64 || arm64) {
		[[]string{}, ['-syscalls'], ['-syscalls', '-vlibc']]
	} $else { [[]string{}] }
	for index, mode in modes {
		binary := os.join_path(root, 'probe_${index}')
		mut arguments := ['-new-compiler', '-no-retry-compilation', '-cc', 'gcc', '-o', binary]
		arguments << mode
		arguments << os.join_path(copy, 'app')
		built := cmdexec.run(@VEXE, arguments)
		assert built.exit_code == 0, built.output
		run := cmdexec.run(binary, [])
		assert run.exit_code == 0, run.output
		assert run.output == 'foreign globals preserve local, parameter, loop, closure, callback and receiver bindings\n', run.output
	}
}
