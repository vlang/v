module test_utils

import os
import net
import time

pub fn set_test_env(test_path string) {
	unbuffer_stdout()
	os.setenv('VMODULES', test_path, true)
	os.setenv('VPM_DEBUG', '', true)
	os.setenv('VPM_NO_INCREMENT', '1', true)
	os.setenv('VPM_FAIL_ON_PROMPT', '1', true)
	// Note: setting a local VTMP here, is *very important*, because VTMP is used for
	// the destination of the temporary clones done by the child `v install` processes.
	// If it is not done, then there is a small chance, that multiple parallel tests
	// can do clones to the same exact folders at the same time, which can make them
	// fail on the CI, with hard to diagnose spurious errors.
	os.setenv('VTMP', os.join_path(test_path, 'vtmp'), true)
}

pub fn hg_serve(hg_path string, path string, start_port int) (&os.Process, int) {
	mut port := start_port
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

pub fn cmd_ok(location string, cmd string) os.Result {
	println('>   cmd_ok for cmd: "${cmd}"')
	res := os.execute(cmd)
	assert res.exit_code == 0, 'success expected, but not found\n    location: ${location}\n    cmd:\n${cmd}\n    res:\n${res}\n'
	return res
}

pub fn cmd_fail(location string, cmd string) os.Result {
	println('> cmd_fail for cmd: "${cmd}"')
	res := os.execute(cmd)
	assert res.exit_code == 1, 'failure expected, but not found\n    location: ${location}\n    cmd:\n${cmd}\n    res:\n${res}\n'
	return res
}
