module test_utils

import os
import net
import time

pub fn set_test_env(test_path string) {
	os.setenv('VMODULES', test_path, true)
	os.setenv('VPM_DEBUG', '', true)
	os.setenv('VPM_NO_INCREMENT', '1', true)
	os.setenv('VPM_FAIL_ON_PROMPT', '1', true)
}

pub fn hg_serve(hg_path string, path string) (&os.Process, int) {
	mut port := 8000
	for {
		if mut l := net.listen_tcp(.ip6, ':${port}') {
			l.close() or { panic(err) }
			break
		}
		port++
	}
	mut p := os.new_process(hg_path)
	p.set_work_folder(path)
	p.set_args(['serve', '--print-url', '--port', port.str()])
	p.set_redirect_stdio()
	p.run()
	mut i := 0
	for p.is_alive() {
		if i == 500 { // Wait max. 5 seconds.
			p.signal_kill()
			eprintln('Failed to serve mercurial repository on localhost.')
			exit(1)
		}
		if p.stdout_read().contains(':${port}') {
			break
		}
		time.sleep(10 * time.millisecond)
		i++
	}
	return p, port
}
