import io
import os
import net.mbedtls

fn main() {
	mut client := mbedtls.new_ssl_conn(mbedtls.SSLConnectConfig{
		verify:   os.resource_abs_path('cert/ca.crt')
		cert:     os.resource_abs_path('cert/client.crt')
		cert_key: os.resource_abs_path('cert/client.key')
		validate: true
	})!

	client.dial('localhost', 8443)!
	client.write_string('GET / HTTP/1.1\r\n\r\n')!
	mut reader := io.new_buffered_reader(reader: client)
	println(reader.read_line()!)
}
