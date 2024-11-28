import io
import os
import net.mbedtls

fn main() {
	mut server := mbedtls.new_ssl_listener('0.0.0.0:8443', mbedtls.SSLConnectConfig{
		verify:   os.resource_abs_path('cert/ca.crt')
		cert:     os.resource_abs_path('cert/server.crt')
		cert_key: os.resource_abs_path('cert/server.key')
		validate: true // mTLS
	})!

	mut client := server.accept()!
	mut reader := io.new_buffered_reader(reader: client)
	mut request := reader.read_line()!
	println(request)
	client.write_string('HTTP/1.1 200 OK\r\n')!
	client.shutdown()!
	server.shutdown()!
}
