import io
import os
import net.mbedtls

// Server Name Indication (SNI) is defined in RFC 6066.
// See https://mbed-tls.readthedocs.io/en/latest/kb/how-to/use-sni/

fn get_cert(mut l mbedtls.SSLListener, host string) !&mbedtls.SSLCerts {
	// callback to return the certificates to use for the connection

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
		verify: os.resource_abs_path('cert/ca.crt')
		cert: os.resource_abs_path('cert/server.crt')
		cert_key: os.resource_abs_path('cert/server.key')
		validate: false
		get_certificate: get_cert // set the SNI callback to enable this feature
	})!

	mut client := server.accept() or {
		println('accept error: ${err}')
		return
	}
	mut reader := io.new_buffered_reader(reader: client)
	mut request := reader.read_line()!
	println(request)
	client.write_string('HTTP/1.1 200 OK\r\n')!
	client.shutdown()!
	server.shutdown()!
}
