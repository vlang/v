import net.mbedtls
import os
import io
import net.http

// first run cert/makecerts.sh to generate the certificates for this server

// start the server with:
// v run server_sni_advanced.v

// test using curl:

// connection using the 1x.dk certificate
// curl -vik --resolve 1x.dk:8443:0.0.0.0 https://1x.dk:8443/

// connection using the 2x.dk certificate
// curl -vik --resolve 2x.dk:8443:0.0.0.0 https://2x.dk:8443/

// default certificate (no sni callback)
// curl -vik https://0.0.0.0:8443/

@[heap]
struct CertManager {
mut:
	// cache for the certificates
	certs map[string]&mbedtls.SSLCerts
}

fn (mut cm CertManager) get_cert(mut l mbedtls.SSLListener, host string) !&mbedtls.SSLCerts {
	println('${host}')

	if c := cm.certs[host] {
		println('read certs for ${host} already ready')
		return c
	} else {
		cert := os.read_file('cert/${host}.crt') or {
			return error('Failed to read certificate ${host}: ${err}')
		}
		key := os.read_file('cert/${host}.key') or {
			return error('Failed to read key ${host}: ${err}')
		}
		println('read certs for ${host}')

		if mut c := mbedtls.new_sslcerts_in_memory('', cert, key) {
			cm.certs[host] = c
			return c
		} else {
			return error('mbedtls.new_sslcerts_in_memory err: ${err}')
		}
	}
}

fn main() {
	// Load the default certificates
	cert := os.read_file('cert/0x.dk.crt') or {
		eprintln('Failed to read certificate: ${err}')
		return
	}
	key := os.read_file('cert/0x.dk.key') or {
		eprintln('Failed to read key: ${err}')
		return
	}

	cm := CertManager{}

	// Create the SSL configuration
	mut config := mbedtls.SSLConnectConfig{
		cert:                   cert
		cert_key:               key
		in_memory_verification: true // !importent
		get_certificate:        cm.get_cert
	}

	mut server := mbedtls.new_ssl_listener('0.0.0.0:8443', config) or {
		println('new_ssl_listener : ${err.msg()} : ${err.code()}')
		return
	}
	println('Listening on https://0.0.0.0:8443')

	// Accept and handle connections
	for {
		mut client := server.accept() or {
			println('accept : ${err.msg()} : ${err.code()}')
			continue
		}
		go handle_connection(mut client)
	}

	server.shutdown() or {
		println('server.shutdown : ${err.msg()} : ${err.code()}')
		return
	}
}

fn handle_connection(mut ssl_conn mbedtls.SSLConn) {
	mut reader := io.new_buffered_reader(reader: ssl_conn)

	request := http.parse_request(mut reader) or {
		println('parse_request failed : ${err}')
		return
	}

	println('Received request: ${request.url}')

	// Respond to the request
	body := 'Hello, HTTPS!'
	res := http.new_response(http.ResponseConfig{
		body: body
	})

	ssl_conn.write(res.bytes()) or {
		eprintln('Failed to write response: ${err}')
		return
	}

	ssl_conn.shutdown() or {
		eprintln('Failed to shutdown connection: ${err}')
		return
	}
}
