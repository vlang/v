module http

import encoding.base64
import net
import net.urllib
import net.ssl
import net.socks

@[heap]
struct HttpProxy {
mut:
	scheme   string
	username string
	password string
	host     string
	hostname string
	port     int
	url      string
}

// new_http_proxy creates a new HttpProxy instance, from the given http proxy url in `raw_url`
pub fn new_http_proxy(raw_url string) !&HttpProxy {
	mut url := urllib.parse(raw_url) or { return error('malformed proxy url') }
	scheme := url.scheme

	if scheme !in ['http', 'https', 'socks5'] {
		return error('invalid scheme')
	}

	url.path = ''
	url.raw_path = ''
	url.raw_query = ''
	url.fragment = ''

	str_url := url.str()

	mut host := url.host
	mut port := url.port().int()

	if port == 0 {
		if scheme == 'https' {
			port = 443
			host += ':' + port.str()
		} else if scheme == 'http' {
			port = 80
			host += ':' + port.str()
		}
	}
	if port == 0 {
		return error('Unknown port')
	}

	return &HttpProxy{
		scheme:   scheme
		username: url.user.username
		password: url.user.password
		host:     host
		hostname: url.hostname()
		port:     port
		url:      str_url
	}
}

// host format - ip:port
fn (pr &HttpProxy) build_proxy_headers(host string) string {
	mut uheaders := []string{}
	address := host.all_before_last(':')
	uheaders << 'Proxy-Connection: Keep-Alive\r\n'
	if pr.username != '' {
		mut authinfo := ''

		authinfo += pr.username
		if pr.password != '' {
			authinfo += ':${pr.password}'
		}

		encoded_authinfo := base64.encode(authinfo.bytes())

		uheaders << 'Proxy-Authorization: Basic ${encoded_authinfo}\r\n'
	}

	version := Version.v1_1

	return 'CONNECT ${host} ${version}\r\nHost: ${address}\r\n' + uheaders.join('') + '\r\n'
}

fn (pr &HttpProxy) http_do(host urllib.URL, method Method, path string, req &Request) !Response {
	host_name, port := net.split_address(host.hostname())!

	s := req.build_request_headers(req.method, host_name, port, path)
	if host.scheme == 'https' {
		mut client := pr.ssl_dial('${host.host}:443')!

		$if windows {
			return error('Windows Not SUPPORTED') // TODO: windows ssl
			// response_text := req.do_request(req.build_request_headers(req.method, host_name,
			// 	path))!
			// client.shutdown()!
			// return response_text
		} $else {
			response_text := req.do_request(req.build_request_headers(req.method, host_name,
				port, path), mut client)!
			client.shutdown()!
			return response_text
		}
	} else if host.scheme == 'http' {
		mut client := pr.dial('${host.host}:80')!
		client.set_read_timeout(req.read_timeout)
		client.set_write_timeout(req.write_timeout)
		client.write_string(s)!
		$if trace_http_request ? {
			eprintln('> ${s}')
		}
		mut bytes := req.read_all_from_client_connection(client)!
		client.close()!
		response_text := bytes.bytestr()
		$if trace_http_response ? {
			eprintln('< ${response_text}')
		}
		if req.on_finish != unsafe { nil } {
			req.on_finish(req, u64(response_text.len))!
		}
		return parse_response(response_text)
	}
	return error('Invalid Scheme')
}

fn (pr &HttpProxy) dial(host string) !&net.TcpConn {
	if pr.scheme in ['http', 'https'] {
		mut tcp := net.dial_tcp(pr.host)!
		tcp.write(pr.build_proxy_headers(host).bytes())!
		mut bf := []u8{len: 4096}

		tcp.read(mut bf)!
		return tcp
	} else if pr.scheme == 'socks5' {
		return socks.socks5_dial(pr.host, host, pr.username, pr.password)!
	} else {
		return error('http_proxy dial: invalid proxy scheme')
	}
}

fn (pr &HttpProxy) ssl_dial(host string) !&ssl.SSLConn {
	if pr.scheme in ['http', 'https'] {
		mut tcp := net.dial_tcp(pr.host)!
		tcp.write(pr.build_proxy_headers(host).bytes())!
		mut bf := []u8{len: 4096}
		tcp.read(mut bf)!
		if !bf.bytestr().contains('HTTP/1.1 200') {
			return error('ssl dial error: ${bf.bytestr()}')
		}

		mut ssl_conn := ssl.new_ssl_conn(
			verify:                 ''
			cert:                   ''
			cert_key:               ''
			validate:               false
			in_memory_verification: false
		)!
		ssl_conn.connect(mut tcp, host.all_before_last(':'))!
		return ssl_conn
	} else if pr.scheme == 'socks5' {
		return socks.socks5_ssl_dial(pr.host, host, pr.username, pr.password)!
	} else {
		return error('http_proxy ssl_dial: invalid proxy scheme')
	}
}
