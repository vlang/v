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

// dial_tcp_via_proxy connects to `host` through the proxy specified by `proxy_url`.
// `host` should be in `host:port` form.
pub fn dial_tcp_via_proxy(proxy_url string, host string) !&net.TcpConn {
	proxy := new_http_proxy(proxy_url)!
	return proxy.dial(host)!
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
	mut username := ''
	mut password := ''

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

	if u := url.user {
		username = u.username
		password = u.password
	}

	return &HttpProxy{
		scheme:   scheme
		username: username
		password: password
		host:     host
		hostname: url.hostname()
		port:     port
		url:      str_url
	}
}

// str returns the configured proxy URL for logging and debugging.
pub fn (pr &HttpProxy) str() string {
	if isnil(pr) {
		return 'nil'
	}
	return pr.url
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

fn read_proxy_connect_response(mut tcp net.TcpConn) !string {
	mut total_bytes_read := 0
	mut msg := [4096]u8{}
	mut buffer := [1]u8{}
	for total_bytes_read < msg.len {
		bytes_read := tcp.read_ptr(&buffer[0], 1)!
		if bytes_read == 0 {
			return error('proxy closed the connection while establishing a tunnel')
		}
		msg[total_bytes_read] = buffer[0]
		total_bytes_read++
		if total_bytes_read > 3 && msg[total_bytes_read - 1] == `\n`
			&& msg[total_bytes_read - 2] == `\r` && msg[total_bytes_read - 3] == `\n`
			&& msg[total_bytes_read - 4] == `\r` {
			return msg[..total_bytes_read].bytestr()
		}
	}
	return error('proxy response headers exceeded 4096 bytes')
}

fn validate_proxy_connect_response(response string) ! {
	status_line := response.all_before('\r\n')
	if !status_line.starts_with('HTTP/1.1 200') && !status_line.starts_with('HTTP/1.0 200') {
		return error('proxy tunnel error: ${status_line}')
	}
}

fn (pr &HttpProxy) connect_tcp(host string) !&net.TcpConn {
	if pr.scheme in ['http', 'https'] {
		mut tcp := net.dial_tcp(pr.host)!
		tcp.write(pr.build_proxy_headers(host).bytes())!
		response := read_proxy_connect_response(mut tcp)!
		validate_proxy_connect_response(response)!
		return tcp
	} else if pr.scheme == 'socks5' {
		return socks.socks5_dial(pr.host, host, pr.username, pr.password)!
	} else {
		return error('http_proxy connect_tcp: invalid proxy scheme')
	}
}

fn (pr &HttpProxy) http_do(host urllib.URL, method Method, path string, req &Request, data string, header Header) !Response {
	host_name := host.hostname()
	mut port := host.port().int()
	if port == 0 {
		port = if host.scheme == 'https' { 443 } else { 80 }
	}
	port_part := if (host.scheme == 'http' && port == 80) || (host.scheme == 'https' && port == 443) {
		''
	} else {
		':${port}'
	}

	s := req.build_request_headers(req.method, host_name, port, '${host.scheme}://${host_name}${port_part}${path}', req.data)
	if host.scheme == 'https' {
		mut client := pr.ssl_dial('${host_name}:${port}')!

		$if windows {
			return error('Windows Not SUPPORTED') // TODO: windows ssl
			// response_text := req.do_request(req.build_request_headers(req.method, host_name,
			// 	path, req.data))!
			// client.shutdown()!
			// return response_text
		} $else {
			response_text := req.do_request(req.build_request_headers(req.method, host_name,
				port, path, req.data), mut client)!
			client.shutdown()!
			return response_text
		}
	} else if host.scheme == 'http' {
		mut client := pr.dial('${host_name}:${port}')!
		client.set_read_timeout(req.read_timeout)
		client.set_write_timeout(req.write_timeout)
		client.write_string(s)!
		$if trace_http_request ? {
			eprintln('> ${s}')
		}
		response_data := req.read_all_from_client_connection(client)!
		client.close()!
		response_text := response_data.data.bytestr()
		$if trace_http_response ? {
			eprintln('< ${response_text}')
		}
		if req.on_finish != unsafe { nil } {
			req.on_finish(req, u64(response_text.len))!
		}
		return parse_received_response(response_text, response_data.info)
	}
	return error('Invalid Scheme')
}

fn (pr &HttpProxy) dial(host string) !&net.TcpConn {
	return pr.connect_tcp(host)!
}

fn (pr &HttpProxy) ssl_dial(host string) !&ssl.SSLConn {
	if pr.scheme in ['http', 'https'] {
		mut tcp := pr.connect_tcp(host)!
		mut ssl_conn := ssl.new_ssl_conn(
			verify:                 ''
			cert:                   ''
			cert_key:               ''
			validate:               false
			in_memory_verification: false
		)!
		ssl_conn.connect(mut tcp, host.all_before_last(':')) or {
			tcp.close() or {}
			return err
		}
		ssl_conn.owns_socket = true
		return ssl_conn
	} else if pr.scheme == 'socks5' {
		return socks.socks5_ssl_dial(pr.host, host, pr.username, pr.password)!
	} else {
		return error('http_proxy ssl_dial: invalid proxy scheme')
	}
}
