module fastc

import os

fn test_discard_prestarted_workers_without_feeding_source() {
	// `fastc_start_capture_input` only prestarts compiler processes on macOS.
	$if !macos {
		return
	}
	tcc := os.join_path(@VEXEROOT, 'thirdparty', 'tcc', 'tcc.exe')
	root := os.join_path(os.vtmp_dir(), 'fastc_unused_workers_${os.getpid()}')
	mut workers := fastc_prestart_c_units(tcc, ['-std=gnu11'], root, 2)
	assert workers.active
	fastc_discard_prestarted_c_units(mut workers)
	assert !workers.active
	assert !os.exists(root)
}

// A prestarted worker waits for its source, so it must not keep a pipe that
// another thread created without FD_CLOEXEC: a reader of that pipe would wait
// for EOF until the worker exits. `os.pipe` creates such a pipe, like os.Process.
fn test_prestarted_worker_does_not_inherit_unrelated_descriptors() {
	$if !macos {
		return
	}
	mut unrelated := os.pipe() or { panic(err) }
	defer {
		unrelated.close()
	}
	script := 'if [ -e /dev/fd/${unrelated.write_fd} ]; then echo inherited; else echo closed; fi'
	mut pid := 0
	mut read_fd := -1
	mut write_fd := -1
	assert fastc_start_capture_input(['/bin/sh', '-c', script], &pid, &read_fd, &write_fd)
	os.fd_close(write_fd)
	output := os.fd_slurp(read_fd).join('').trim_space()
	os.fd_close(read_fd)
	assert fastc_wait_exit_code(pid) == 0
	assert output == 'closed'
}
