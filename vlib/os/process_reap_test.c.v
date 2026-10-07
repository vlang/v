module os

import time

fn test_signal_kill_then_wait_reaps_child() {
	$if windows {
		return
	} $else {
		mut p := new_process(find_abs_path_of_executable('sleep')!)
		p.set_args(['30'])
		p.run()
		defer {
			mut status := 0
			C.waitpid(p.pid, &status, 0)
			p.close()
		}
		p.signal_kill()
		assert p.status == .aborted
		p.wait()
		assert p.status == .aborted
		assert p.code == 128 + int(C.SIGKILL)
		code := p.code
		message := p.err
		p.wait()
		assert p.code == code
		assert p.err == message
		mut status := 0
		assert C.waitpid(p.pid, &status, C.WNOHANG) == -1
		assert C.errno == C.ECHILD
	}
}

fn test_wait_after_is_alive_reaps_a_signaled_child() {
	$if windows {
		return
	} $else {
		mut p := new_process(find_abs_path_of_executable('sleep')!)
		p.set_args(['30'])
		p.run()
		defer {
			mut status := 0
			C.waitpid(p.pid, &status, 0)
			p.close()
		}
		assert C.kill(p.pid, C.SIGKILL) == 0
		for _ in 0 .. 500 {
			if !p.is_alive() {
				break
			}
			time.sleep(10 * time.millisecond)
		}
		assert p.status == .aborted
		code := p.code
		message := p.err
		p.wait()
		p.wait()
		assert p.code == code
		assert p.err == message
		mut status := 0
		assert C.waitpid(p.pid, &status, C.WNOHANG) == -1
		assert C.errno == C.ECHILD
	}
}
