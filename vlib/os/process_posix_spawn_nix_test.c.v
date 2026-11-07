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
	p.set_stdin_path('/dev/null')
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
