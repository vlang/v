import io
import os
import net.mbedtls

// This example shows a very minimal HTTP server, that supports SNI.
// Server Name Indication (SNI) is defined in RFC 6066.
// See https://mbed-tls.readthedocs.io/en/latest/kb/how-to/use-sni/

// Use the following commands, in separate shells, to test that it works,
// after you start the server:
//   curl -ik --resolve 1x.dk:8443:0.0.0.0        https://1x.dk:8443/abcd
//   curl -ik --resolve example.com:8443:0.0.0.0  https://example.com:8443/xyz
// Both of them should work, and the server should show that it tried to load
// certificates, responding to the given domain name in each of the requests.

// callback to return the certificates to use for the connection
fn get_cert(mut l mbedtls.SSLListener, host string) !&mbedtls.SSLCerts {
	println('reading certificates for ${host} ...')

	// For our example, just always use the same certificates. In a real server,
	// that should be dependent on the host parameter, and perhaps there should
	// be some caching, so that they are not read each time for each request from
	// the filesystem.

	cert := os.read_file('cert/server.crt') or {
		return error('Failed to read certificate ${host}: ${err}')
	}
	key := os.read_file('cert/server.key') or { return error('Failed to read key ${host}: ${err}') }
	println('read certs for ${host}')

	if mut c := mbedtls.new_sslcerts_in_memory('', cert, key) {
		return c
	} else {
		return error('mbedtls.new_sslcerts_in_memory err: ${err}')
	}
}

fn main() {
	mut server := mbedtls.new_ssl_listener('0.0.0.0:8443', mbedtls.SSLConnectConfig{
		verify:          os.resource_abs_path('cert/ca.crt')
		cert:            os.resource_abs_path('cert/server.crt')
		cert_key:        os.resource_abs_path('cert/server.key')
		validate:        false
		get_certificate: get_cert // set the SNI callback to enable this feature
	})!
	println('Listening on https://0.0.0.0:8443/ ...')
	for {
		mut client := server.accept() or {
			println('accept error: ${err}')
			continue
		}
		mut reader := io.new_buffered_reader(reader: client)
		for {
			line := reader.read_line() or { break }
			if line.len == 0 {
				break
			}
			println(line)
		}
		client.write_string('HTTP/1.1 200 OK\r\n') or { continue }

		client.write_string('Content-Type: text/html\r\n') or { continue }
		client.write_string('Connection: close\r\n') or { continue }
		client.write_string('\r\n') or { continue }

		client.write_string('Hello\n') or { continue }
		client.shutdown()!
	}
	server.shutdown()!
}
