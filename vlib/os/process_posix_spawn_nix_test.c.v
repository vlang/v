// vtest build: linux
module os

// posix_spawn_pipes returns a pipe set laid out like the one of unix_spawn_process.
fn posix_spawn_pipes() [6]i32 {
	mut pipeset := [6]i32{}
	C.pipe(&pipeset[0])
	C.pipe(&pipeset[2])
	return pipeset
}

fn test_posix_spawn_runs_a_redirected_child_in_its_own_group() {
	$if !linux {
		return
	}
	mut p := new_process(find_abs_path_of_executable('sh')!)
	p.set_args(['-c', 'echo out; echo err >&2; ps -o pgid= -p $$'])
	p.set_redirect_stdio_merged()
	p.use_pgroup = true
	stdin_file := join_path(vtmp_dir(), 'posix_spawn_stdin_${getpid()}.txt')
	write_file(stdin_file, 'unread\n')!
	defer {
		rm(stdin_file) or {}
	}
	p.set_stdin_path(stdin_file)
	plan := p.unix_prepare_spawn()
	mut pipeset := posix_spawn_pipes()
	pid := p.unix_try_posix_spawn(plan, pipeset)
	assert pid > 0
	fd_close(pipeset[3])
	text := fd_slurp(pipeset[2]).join('\n')
	mut status := 0
	C.waitpid(pid, &status, 0)
	fd_close(pipeset[2])
	fd_close(pipeset[0])
	fd_close(pipeset[1])
	assert text.contains('out')
	assert text.contains('err')
	// The child leads its own process group: its group id is its pid.
	assert text.trim_space().ends_with(pid.str()), text
}

fn test_posix_spawn_leaves_special_spawns_to_fork() {
	$if !linux {
		return
	}
	mut with_folder := new_process(find_abs_path_of_executable('true')!)
	with_folder.set_work_folder(temp_dir())
	assert with_folder.unix_try_posix_spawn(with_folder.unix_prepare_spawn(), [6]i32{}) == -1
	mut missing := new_process('definitely-not-an-installed-program-5f3a')
	assert missing.unix_try_posix_spawn(missing.unix_prepare_spawn(), [6]i32{}) == -1
}

// posix_spawn_fifo makes a fresh FIFO for one test and returns its path.
fn posix_spawn_fifo(name string) string {
	path := join_path(vtmp_dir(), 'posix_spawn_${name}_${getpid()}.fifo')
	rm(path) or {}
	assert C.mkfifo(&char(path.str), 0o600) == 0
	return path
}

// posix_spawn runs the open of stdin as a file action, and on Linux its caller waits until
// the actions are done. The open of a FIFO waits for a writer: with posix_spawn the call
// would not return before one came, and the writer may be the caller itself, after run().
fn test_posix_spawn_leaves_a_stdin_that_is_not_a_regular_file_to_fork() {
	$if !linux {
		return
	}
	cat := find_abs_path_of_executable('cat')!
	fifo := posix_spawn_fifo('selector')
	defer {
		rm(fifo) or {}
	}
	for path in [fifo, '/dev/null', join_path(vtmp_dir(), 'posix_spawn_no_such_stdin_5f3a')] {
		mut p := new_process(cat)
		p.set_stdin_path(path)
		assert p.unix_try_posix_spawn(p.unix_prepare_spawn(), [6]i32{}) == -1, path
	}
}

fn test_run_returns_before_a_fifo_stdin_has_a_writer() {
	$if !linux {
		return
	}
	fifo := posix_spawn_fifo('run')
	defer {
		rm(fifo) or {}
	}
	mut p := new_process(find_abs_path_of_executable('cat')!)
	p.set_stdin_path(fifo)
	p.set_redirect_stdio()
	p.run()
	// run() has returned with the child waiting in its open: the writer starts only now
	write_file(fifo, 'fifo payload\n')!
	p.wait()
	output := p.stdout_slurp()
	p.close()
	assert p.code == 0
	assert output == 'fifo payload\n'
}

// The object with `v_os_posix_spawn` is linked on hosted Linux only. A target without it must
// not refer to the function at all, also when something there calls unix_try_posix_spawn, as
// this file does: the C that V generates for macOS from this file has the helper and no call
// of the wrapper.
fn test_other_targets_do_not_refer_to_the_linux_only_wrapper() {
	$if !linux {
		return
	}
	generated := join_path(vtmp_dir(), 'posix_spawn_macos_${getpid()}.c')
	defer {
		rm(generated) or {}
	}
	result := execute('${quoted_path(@VEXE)} -os macos -o ${quoted_path(generated)} ${quoted_path(@FILE)}')
	assert result.exit_code == 0, result.output
	c := read_file(generated)!
	assert c.contains('unix_try_posix_spawn'), 'the helper is not in the generated C: the check below would prove nothing'
	// Joined when the test runs: as one literal, or as two that the compiler folds into one,
	// the name would be in the generated C as this very string.
	wrapper_call := ['v_os_posix', 'spawn('].join('_')
	assert !c.contains(wrapper_call), 'C generated for macOS refers to the Linux-only wrapper'
}
