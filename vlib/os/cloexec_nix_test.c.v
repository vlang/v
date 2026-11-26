import os

// The pipes of one Process must reach only its own child. A later child,
// here a shell that reports which descriptors above 2 it can read from,
// must not inherit the ends the parent keeps for the first process.
fn test_a_later_child_does_not_inherit_another_process_pipes() {
	sh := os.find_abs_path_of_executable('sh')!
	mut held := os.new_process(sh)
	held.set_args(['-c', 'read value'])
	held.set_redirect_stdio()
	held.run()
	defer {
		held.stdin_write('done\n')
		held.wait()
		held.close()
	}
	mut probe := os.new_process(sh)
	probe.set_args(['-c',
		'for fd in 3 4 5 6 7 8 9; do (: <&\$fd) 2>/dev/null && printf "%s " \$fd; done; true'])
	probe.set_redirect_stdio()
	probe.run()
	probe.wait()
	inherited := probe.stdout_slurp().trim_space()
	probe.close()
	// The probe's own stdout pipe is descriptor 1 in it; nothing else should be open.
	// (Descriptors from 10 up are the shell's own while it tests a redirection.)
	assert inherited == '', 'inherited descriptors: ${inherited}'
}

// Files V opens are close-on-exec as well: a program spawned by another
// thread while one is open must not inherit it.
fn test_files_opened_by_os_are_close_on_exec() {
	path := os.join_path(os.vtmp_dir(), 'cloexec_${os.getpid()}.txt')
	os.write_file(path, 'data')!
	defer {
		os.rm(path) or {}
	}
	mut opened := os.open(path)!
	assert C.fcntl(opened.fd, C.F_GETFD) & C.FD_CLOEXEC != 0
	opened.close()
	mut appended := os.open_append(path)!
	assert C.fcntl(appended.fd, C.F_GETFD) & C.FD_CLOEXEC != 0
	appended.close()
	mut created := os.open_file(path, 'w')!
	assert C.fcntl(created.fd, C.F_GETFD) & C.FD_CLOEXEC != 0
	created.close()
}
