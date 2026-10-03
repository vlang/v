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
	// The records of what `v install --local` installed live in the user's cache,
	// deliberately away from VMODULES. Point them at the test's own directory --
	// rather than moving the whole cache, which the compiled tools also live in.
	os.setenv('VPM_LOCAL_INSTALLS', os.join_path(test_path, 'local_installs'), true)
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

// cmd_ok checks the exit status of a legacy command string.
@[deprecated: 'use cmd_ok_args with an argument array to avoid shell injection']
pub fn cmd_ok(location string, cmd string) os.Result {
	return cmd_ok_args(location, os.split_args(cmd) or { panic(err) })
}

// cmd_ok_args checks the exit status of a program with literal arguments.
pub fn cmd_ok_args(location string, args []string) os.Result {
	cmd := args.map(os.quoted_path(it)).join(' ')
	println('>   cmd_ok for cmd: "${cmd}"')
	res := os.exec(args)
	assert res.exit_code == 0, 'success expected, but not found\n    location: ${location}\n    cmd:\n${cmd}\n    res:\n${res}\n'
	return res
}

// cmd_fail checks the exit status of a legacy command string.
@[deprecated: 'use cmd_fail_args with an argument array to avoid shell injection']
pub fn cmd_fail(location string, cmd string) os.Result {
	return cmd_fail_args(location, os.split_args(cmd) or { panic(err) })
}

// cmd_fail_args checks the exit status of a program with literal arguments.
pub fn cmd_fail_args(location string, args []string) os.Result {
	cmd := args.map(os.quoted_path(it)).join(' ')
	println('> cmd_fail for cmd: "${cmd}"')
	res := os.exec(args)
	assert res.exit_code == 1, 'failure expected, but not found\n    location: ${location}\n    cmd:\n${cmd}\n    res:\n${res}\n'
	return res
}
