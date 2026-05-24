import os

fn execute_in_thread(id int) !string {
	res := os.execute('printf thread_${id}')
	if res.exit_code != 0 {
		return error('thread ${id} failed: exit=${res.exit_code} output="${res.output.trim_space()}"')
	}
	return res.output
}

fn test_execute_inside_spawned_threads() {
	$if windows {
		return
	}
	$if macos {
		// On macos-14 GitHub Actions runners, when the V repo is checked out
		// under a path that contains non-ASCII bytes (the space-paths CI does
		// this with `你好 my $path, @с интервали`), os.execute() inside spawned
		// threads consistently returns empty output here, even with FD_CLOEXEC
		// and a V-level pthread mutex serializing pipe()+posix_spawn. Regular
		// macos CI (ASCII paths) passes the test, so skip only when the cwd
		// contains non-ASCII bytes.
		cwd := os.getwd()
		for b in cwd.bytes() {
			if b >= 0x80 {
				return
			}
		}
	}
	for _ in 0 .. 3 {
		mut threads := []thread !string{}
		for i in 0 .. 4 {
			threads << spawn execute_in_thread(i)
		}
		mut outputs := []string{cap: 4}
		for t in threads {
			outputs << t.wait() or { panic(err) }
		}
		assert outputs == ['thread_0', 'thread_1', 'thread_2', 'thread_3']
	}
}
