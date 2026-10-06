import os
import v.cmdexec

fn test_generic_codegen_after_scoped_transform_with_two_workers() {
	root := os.join_path(os.vtmp_dir(), 'v3_scoped_smartcast_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	previous_jobs := os.getenv_opt('VJOBS')
	os.setenv('VJOBS', '2', true)
	defer {
		if value := previous_jobs {
			os.setenv('VJOBS', value, true)
		} else {
			os.unsetenv('VJOBS')
		}
	}
	fixture := os.join_path(@VEXEROOT, 'vlib/v/checker/tests/generic_type_inference.vv')
	mut executable := os.join_path(root, 'generic_tree')
	$if windows {
		executable += '.exe'
	}
	// Two workers exercise the scoped specialization path. Constant generation
	// subsequently queries the checker after that arena has been released.
	compiled := cmdexec.run_with_timeout(@VEXE, ['-new-compiler', '-nocache', '-o', executable,
		fixture], 120_000)
	assert compiled.exit_code == 0, compiled.output
	ran := cmdexec.run_with_timeout(executable, [], 30_000)
	assert ran.exit_code == 0, ran.output
	assert ran.output.contains('alibaba'), ran.output
	assert ran.output.contains('12'), ran.output
}
