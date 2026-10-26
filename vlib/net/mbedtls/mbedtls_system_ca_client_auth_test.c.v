module mbedtls

import net
import os
import time

fn test_system_ca_client_without_credentials_accepts_optional_client_auth() ! {
	$if macos || linux {
		cert_dir := os.join_path(@VMODROOT, 'examples', 'ssl_server', 'cert')
		previous_ca := os.getenv_opt('SSL_CERT_FILE')
		os.setenv('SSL_CERT_FILE', os.join_path(cert_dir, 'ca.crt'), true)
		defer {
			if value := previous_ca {
				os.setenv('SSL_CERT_FILE', value, true)
			} else {
				os.unsetenv('SSL_CERT_FILE')
			}
		}
		mut listener := new_ssl_listener('127.0.0.1:0', SSLConnectConfig{
			cert:     os.join_path(cert_dir, 'server.crt')
			cert_key: os.join_path(cert_dir, 'server.key')
		})!
		defer {
			listener.shutdown() or {}
		}
		C.mbedtls_ssl_conf_authmode(&listener.conf, C.MBEDTLS_SSL_VERIFY_OPTIONAL)
		port := net.addr_from_socket_handle(listener.server_fd.fd).port()!
		mut client := new_ssl_conn(
			validate:     true
			read_timeout: 10 * time.second
		)!
		defer {
			client.shutdown() or {}
		}
		server := spawn fn [mut listener] () string {
			mut conn := listener.accept_with_timeouts(10 * time.second, 10 * time.second) or {
				return err.msg()
			}
			conn.shutdown() or { return err.msg() }
			return ''
		}()
		client.dial('localhost', port) or {
			server_error := server.wait()
			return error('${err}; server: ${server_error}')
		}
		server_error := server.wait()
		client.shutdown()!
		assert server_error == ''
	}
}
