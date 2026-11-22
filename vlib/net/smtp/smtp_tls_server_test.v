// vtest build: present_openssl? && !(windows && tinyc)
module smtp

import net
import net.smtp.testdata.tls_server
import os
import rand
import time

const smtp_server_probe = '--vtest-smtp-tls-server'

fn testsuite_begin() {
	if os.args.len > 1 && os.args[1] == smtp_server_probe {
		tls_server.run(os.args[2..])
		exit(0)
	}
}

fn test_tls_server_exits_when_no_client_connects() ! {
	scratch := os.join_path(os.vtmp_dir(), 'smtp_tls_server_${rand.uuid_v4()}')
	os.mkdir_all(scratch)!
	defer {
		os.rmdir_all(scratch) or {}
	}
	ready_path := os.join_path(scratch, 'ready')
	result_path := os.join_path(scratch, 'result')
	cert_dir := os.join_path(@VMODROOT, 'examples', 'ssl_server', 'cert')
	mut server := os.new_process(os.executable())
	// No client will connect, so this probe can exercise the accept timeout
	// without the longer startup budget used by the TLS client tests.
	server.set_args([smtp_server_probe, 'starttls', '127.0.0.1', os.join_path(cert_dir, 'server.crt'),
		os.join_path(cert_dir, 'server.key'), ready_path, result_path, '250'])
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
	exit_deadline := time.now().add(5 * time.second)
	for server.is_alive() && time.now() < exit_deadline {
		time.sleep(20 * time.millisecond)
	}
	assert !server.is_alive(), 'SMTP TLS test server waited indefinitely without a client'
	server.wait()
	assert server.code != 0
	stderr := server.stderr_slurp()
	assert stderr.contains('accept failed: ${net.err_timed_out.msg()}'), stderr
	assert !os.exists(result_path)
}
