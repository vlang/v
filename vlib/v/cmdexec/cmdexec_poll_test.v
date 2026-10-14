module cmdexec

import os

fn test_cmdexec_keeps_native_poll_storage_with_warm_module_cache() {
	$if windows {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'cmdexec_native_poll_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.c.v')
	cache := os.join_path(root, 'cache')
	for marker in ['cold', 'warm'] {
		os.write_file(source, '#include <poll.h>\nimport v.cmdexec\nfn main() {\n result := cmdexec.run_with_timeout("sh", ["-c", "printf ${marker}"], 5000)\n assert result.exit_code == 0, result.output\n assert result.output == "${marker}", result.output\n println("OK")\n}\n')!
		run := cmdexec.run('env', ['V3CACHE=${cache}', @VEXE, '-new-compiler', '-cc', 'clang',
			'-no-retry-compilation', 'run', source])
		assert run.exit_code == 0, run.output
		assert run.output == 'OK\n', run.output
	}
}

fn test_cmdexec_owned_poll_can_coexist_with_application_poll_bindings() {
	$if windows {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'cmdexec_owned_poll_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	// Unsuffixed source lets V supply the complete headerless C record layout.
	source := os.join_path(root, 'main.v')
	os.write_file(source, '#flag -lc\nimport v.cmdexec\nstruct C.pollfd { fd i32 events i16 revents i16 }\n$if macos || ios || freebsd || openbsd || dragonfly || android || termux { fn C.poll(fds &C.pollfd, nfds u32, timeout i32) i32 } $else { fn C.poll(fds &C.pollfd, nfds usize, timeout i32) i32 }\nfn main() {\n mut descriptor := C.pollfd{fd: -1, events: 1}\n assert C.poll(&descriptor, 1, 0) == 0\n assert cmdexec.run_with_timeout("sh", ["-c", "sleep 0.01; echo OK"], 5000).exit_code == 0\n println("OK")\n}\n')!
	mut compilers := ['cc']
	$if macos || linux {
		compilers << 'tcc'
	}
	for cc in compilers {
		run := cmdexec.run(@VEXE, ['-new-compiler', '-nocache', '-cc', cc, '-no-retry-compilation',
			'run', source])
		assert run.exit_code == 0, run.output
		assert run.output == 'OK\n', run.output
	}
}
