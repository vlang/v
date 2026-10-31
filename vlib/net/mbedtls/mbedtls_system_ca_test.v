module mbedtls

import net
import os

const test_cert_dir = os.join_path(@VMODROOT, 'examples', 'ssl_server', 'cert')

fn test_system_ca_file_prefers_ssl_cert_file() {
	ca_path := os.join_path(test_cert_dir, 'ca.crt')
	os.setenv('SSL_CERT_FILE', ca_path, true)
	defer {
		os.unsetenv('SSL_CERT_FILE')
	}
	assert system_ca_file() == ca_path
}

fn test_validate_without_verify_uses_the_system_ca_bundle() ! {
	$if macos || linux {
		os.setenv('SSL_CERT_FILE', os.join_path(test_cert_dir, 'ca.crt'), true)
		defer {
			os.unsetenv('SSL_CERT_FILE')
		}
		mut listener := new_ssl_listener('127.0.0.1:0', SSLConnectConfig{
			cert:     os.join_path(test_cert_dir, 'server.crt')
			cert_key: os.join_path(test_cert_dir, 'server.key')
		})!
		defer {
			listener.shutdown() or {}
		}
		port := net.addr_from_socket_handle(listener.server_fd.fd).port()!
		server := spawn fn [mut listener] () {
			mut conn := listener.accept() or { return }
			conn.shutdown() or {}
		}()
		mut client := new_ssl_conn(validate: true)!
		client.dial('localhost', port)!
		client.shutdown()!
		server.wait()
	}
}
