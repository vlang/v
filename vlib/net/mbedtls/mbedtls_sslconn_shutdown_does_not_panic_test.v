import net.mbedtls
import time

$if network ? {
	import net
}

const shutdown_test_cert = $embed_file('../testdata/localhost_test_cert.pem').to_string()

const shutdown_test_key = $embed_file('../testdata/localhost_test_key.pem').to_string()

fn new_shutdown_test_listener(port int) !&mbedtls.SSLListener {
	cfg := mbedtls.SSLConnectConfig{
		cert:                   shutdown_test_cert
		cert_key:               shutdown_test_key
		validate:               false
		in_memory_verification: true
	}
	return mbedtls.new_ssl_listener('127.0.0.1:${port}', cfg)
}

fn shutdown_test_connection(mut srv mbedtls.SSLListener) ! {
	mut cli := srv.accept_with_timeout(5 * time.second)!
	cli.shutdown()!
	// A second shutdown must be a harmless no-op (idempotent): SSLConn.shutdown
	// sets `opened = false` BEFORE freeing, so a second call hits the early
	// `if !s.opened` return instead of a second pass through the mbedtls frees.
	// The guard's real purpose is preventing the double-free that the
	// worker-defer-vs-close_idle race would otherwise cause (a double mbedtls_*_free
	// aborts the process — or, on a forgiving allocator, silently corrupts the heap
	// without erroring). Asserting merely that *an* error was returned would not
	// distinguish the idempotent no-op from a second free that happened to report
	// something; assert the specific "connection was not open" sentinel so the test
	// proves the early-return path was taken.
	mut second_err := ''
	cli.shutdown() or { second_err = err.msg() }
	assert second_err.contains('connection was not open'), 'second shutdown must hit the idempotent !opened early-return (sentinel "connection was not open"), proving no second free; got: ${second_err}'
}

@[if network ?]
fn test_shutdown_does_not_panic() {
	mut port_listener := net.listen_tcp(.ip, '127.0.0.1:0')!
	port := port_listener.addr()!.port()!
	port_listener.close()!
	mut srv := new_shutdown_test_listener(port)!
	defer {
		srv.shutdown() or {}
	}
	mut client := mbedtls.new_ssl_conn(validate: false)!
	defer {
		client.shutdown() or {}
	}
	worker := spawn shutdown_test_connection(mut srv)
	client.dial('127.0.0.1', port) or {
		worker.wait() or {}
		assert false, 'dial: ${err}'
		return
	}
	worker.wait()!
}
