module main

import net
import net.http
import os
import time

const startup_root = os.join_path(os.temp_dir(), 'vpm_registry_startup_${os.getpid()}')
const startup_executable = os.join_path(startup_root, if os.user_os() == 'windows' {
	'vpm.exe'
} else {
	'vpm'
})

fn testsuite_begin() {
	os.mkdir_all(startup_root)!
	os.setenv('CI', '1', true)
	os.setenv('VMODULES', os.join_path(startup_root, 'modules'), true)
	result := os.exec([@VEXE, '-cc', @CCOMPILER, '-gc', 'none', '-o', startup_executable,
		os.join_path(@VEXEROOT, 'cmd', 'tools', 'vpm')])
	assert result.exit_code == 0, result.output
}

fn testsuite_end() {
	os.rmdir_all(startup_root) or {}
}

fn start_registry_process(port int) &os.Process {
	mut process := os.new_process(startup_executable)
	process.set_args(['registry', 'serve', '--port', port.str()])
	process.set_work_folder(startup_root)
	process.set_redirect_stdio()
	process.run()
	return process
}

fn stop_registry_process(mut process os.Process) {
	if process.is_alive() {
		process.signal_kill()
	}
	process.wait()
	process.close()
}

fn test_registry_reports_an_occupied_port_without_claiming_startup() {
	$if windows {
		// net.listen_tcp enables SO_REUSEADDR, which can allow a second Windows
		// listener on this port. Its public API cannot reserve an exclusive socket.
		return
	}
	mut listener := net.listen_tcp(.ip, ':0')!
	defer { listener.close() or {} }
	port := listener.addr()!.port()!
	mut process := start_registry_process(port)
	defer { stop_registry_process(mut process) }
	for _ in 0 .. 500 {
		if !process.is_alive() {
			break
		}
		time.sleep(10 * time.millisecond)
	}
	assert !process.is_alive(), 'registry did not report the occupied port'
	process.wait()
	output := process.stdout_slurp() + process.stderr_slurp()
	assert process.code == 1, output
	assert output.contains('failed to listen on port ${port}'), output
	assert !output.contains('Serving '), output
	assert !output.contains('Listening on http://'), output
}

fn test_registry_reports_startup_after_binding_and_serves_requests() {
	mut listener := net.listen_tcp(.ip, ':0')!
	port := listener.addr()!.port()!
	listener.close()!
	mut process := start_registry_process(port)
	defer { stop_registry_process(mut process) }
	mut output := ''
	for _ in 0 .. 500 {
		output += process.stdout_read()
		if output.contains('Listening on http://') || !process.is_alive() {
			break
		}
		time.sleep(10 * time.millisecond)
	}
	assert process.is_alive(), output + process.stderr_read()
	assert output.contains('Serving 0 module(s) on port ${port}'), output
	assert output.contains('Listening on http://127.0.0.1:${port}'), output
	response := http.fetch(
		url:           'http://127.0.0.1:${port}/config.json'
		read_timeout:  2 * time.second
		write_timeout: 2 * time.second
	)!
	assert response.status_code == 200
}
