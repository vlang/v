// vtest build: present_openssl? && !(windows && tinyc)
module smtp

import os
import rand
import time

fn test_tls_server_exits_when_no_client_connects() ! {
	scratch := os.join_path(os.vtmp_dir(), 'smtp_tls_server_${rand.uuid_v4()}')
	os.mkdir_all(scratch)!
	defer {
		os.rmdir_all(scratch) or {}
	}
	helper := os.join_path(@VMODROOT, 'vlib', 'net', 'smtp', 'testdata', 'smtp_tls_test_server.v')
	executable := os.join_path(scratch, 'smtp_tls_test_server') + $if windows { '.exe' } $else { '' }
	mut build := os.new_process(@VEXE)
	build.set_args(['-o', executable, helper])
	build.set_redirect_stdio()
	build.wait()
	build_error := build.stderr_slurp()
	build.close()
	assert build.code == 0, build_error

	ready_path := os.join_path(scratch, 'ready')
	result_path := os.join_path(scratch, 'result')
	cert_dir := os.join_path(@VMODROOT, 'examples', 'ssl_server', 'cert')
	mut server := os.new_process(executable)
	server.set_args(['starttls', '127.0.0.1', os.join_path(cert_dir, 'server.crt'),
		os.join_path(cert_dir, 'server.key'), ready_path, result_path])
	server.set_redirect_stdio()
	server.run()
	defer {
		if server.is_alive() {
			server.signal_kill()
		}
		server.wait()
		server.close()
	}

	started := time.now()
	for !os.exists(ready_path) && server.is_alive() && time.since(started) < 5 * time.second {
		time.sleep(10 * time.millisecond)
	}
	assert os.exists(ready_path), 'SMTP TLS test server did not publish its port'
	port := os.read_file(ready_path)!.int()
	assert port > 0 && port <= 65535
	assert !os.exists('${ready_path}.tmp')
	for server.is_alive() && time.since(started) < 15 * time.second {
		time.sleep(20 * time.millisecond)
	}
	assert !server.is_alive(), 'SMTP TLS test server waited indefinitely without a client'
	server.wait()
	assert server.code != 0
	assert server.stderr_slurp().contains('accept failed:')
	assert !os.exists(result_path)
}
