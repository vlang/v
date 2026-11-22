// vtest build: !gcc-windows && !gcc-windows-self-tests // too flaky on these CIs
// vtest retry: 3
import io
import os
import time
import net.unix
import net as _

const tfolder = os.join_path(os.temp_dir(), 'nuut_${os.getpid()}')
const test_port = os.join_path(tfolder, 'domain_socket')

fn testsuite_begin() {
	os.mkdir_all(tfolder) or {}
	spawn fn () {
		// Normally this entire test should take less than a second,
		// but sometimes it hangs on windows for hours. Instead of skipping it entirely,
		// ensure the test will die, and later the test framework can restart it.
		time.sleep(5 * time.second)
		exit(1)
	}()
}

fn testsuite_end() {
	os.rmdir_all(tfolder) or {}
}

fn test_that_net_and_net_unix_can_be_imported_together_without_conflicts() {
	mut listener := unix.listen_stream(test_port)!
	defer { listener.close() or {} }
	listener.set_accept_timeout(2 * time.second)

	mut client := unix.connect_stream(test_port)!
	defer { client.close() or {} }
	mut server := listener.accept()!
	defer { server.close() or {} }
	client.set_read_timeout(2 * time.second)
	server.set_read_timeout(2 * time.second)

	data := 'Hello from vlib/net!'
	client.write_string(data)!
	mut request := []u8{len: data.len}
	io.read_exact(mut server, mut request)!
	server.write(request)!

	mut response := []u8{len: data.len}
	io.read_exact(mut client, mut response)!
	assert response == data.bytes()
}
