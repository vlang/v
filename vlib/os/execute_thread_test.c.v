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
