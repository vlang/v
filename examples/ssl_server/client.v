import io
import net.mbedtls

fn main() {
	mut client := mbedtls.new_ssl_conn(mbedtls.SSLConnectConfig {
		verify: "cert/ca.crt"
		cert: "cert/client.crt"
		cert_key: "cert/client.key"
		validate: true
	})!

	client.dial("localhost", 8443)!
	client.write_string("GET / HTTP/1.1\r\n\r\n")!
	mut reader := io.new_buffered_reader(reader: client)
	println(reader.read_line()!)
}
