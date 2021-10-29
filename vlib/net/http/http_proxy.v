module http

import io
import os
import net
import net.urllib
import encoding.base64
import time

// would be nice to have this as an interface..
struct ProxyConnLayer {
pub mut:
	fd            int = -1
	read_timeout  time.Duration = 30 * time.second
	write_timeout time.Duration = 30 * time.second
}

fn (mut c ProxyConnLayer) read(mut buf []byte) ?int {
	mut res := C.recv(c.fd, voidptr(buf.data), buf.len, 0)
	if res > 0 {
		return res
	}

	code := net.error_code()
	if code == int(net.error_ewouldblock) {
		net.wait_for_read(c.fd, time.unix(0), c.read_timeout) ?

		res = C.recv(c.fd, voidptr(buf.data), buf.len, 0)

		if res > 0 {
			return res
		}
	} else {
		net.wrap_error(code) ?
	}

	return none
}

fn (mut c ProxyConnLayer) write(bytes []byte) ?int {
	unsafe {
		b := bytes.data
		len := bytes.len

		mut ptr_base := &byte(b)
		mut total_sent := 0
		for total_sent < len {
			ptr := ptr_base + total_sent
			remaining := len - total_sent
			mut sent := C.send(c.fd, ptr, remaining, net.msg_nosignal)
			if sent < 0 {
				code := net.error_code()
				if code == int(net.error_ewouldblock) {
					net.wait_for_write(c.fd, time.unix(0), c.write_timeout) ?
					continue
				} else {
					net.wrap_error(code) ?
				}
			}
			total_sent += sent
		}
		return total_sent
	}
}

fn (mut c ProxyConnLayer) close() ? {
	if os.fd_close(c.fd) != 0 {
		return error('could not close ProxyConnLayer')
	}
}

fn (mut c ProxyConnLayer) set_read_timeout(timeout time.Duration) {
	c.read_timeout = timeout
}

fn (mut c ProxyConnLayer) set_write_timeout(timeout time.Duration) {
	c.write_timeout = timeout
}

struct HttpProxy {
mut:
	scheme   string
	userinfo urllib.Userinfo
	host     string
	hostname string
	port     int
	url      string

	validate               bool = true
	verify                 string
	cert                   string
	cert_key               string
	in_memory_verification bool

	has_conn    bool
	remote_host string
pub mut:
	conn ProxyConnLayer = ProxyConnLayer{}
}

pub fn new_http_proxy(raw_url string) ?HttpProxy {
	mut url := urllib.parse(raw_url) ?
	scheme := url.scheme

	if scheme != 'http' && scheme != 'https' {
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
		} else if scheme == 'http' {
			port = 80
		}

		host += ':' + port.str()
	}

	return HttpProxy{
		scheme: scheme
		userinfo: url.user
		host: host
		hostname: url.hostname()
		port: port
		url: str_url
	}
}

fn (proxy HttpProxy) build_proxy_headers(request &Request, host string) string {
	ua := request.user_agent
	mut uheaders := []string{}

	uheaders << 'Host: $host\r\n'
	uheaders << 'User-Agent: $ua\r\n'
	uheaders << 'Proxy-Connection: Keep-Alive\r\n'

	if proxy.userinfo.username != '' {
		mut authinfo := ''

		authinfo += proxy.userinfo.username
		if proxy.userinfo.password_set {
			authinfo += ':' + proxy.userinfo.password
		}

		encoded_authinfo := base64.encode(authinfo.bytes())

		uheaders << 'Proxy-Authorization: Basic $encoded_authinfo\r\n'
	}

	version := Version.v1_1

	return 'CONNECT $host $version\r\n' + uheaders.join('') + '\r\n'
}

fn (mut proxy HttpProxy) create_tcp_layer() ?ProxyConnLayer {
	if proxy.scheme == 'http' {
		tcp_connection := net.dial_tcp(proxy.host) ?
		return ProxyConnLayer{
			fd: tcp_connection.sock.handle
		}
	} else if proxy.scheme == 'https' {
		return proxy.create_ssl_tcp(proxy.hostname, proxy.port)
	} else {
		return error('wrong schema')
	}
}

pub fn (mut proxy HttpProxy) prepare(request &Request, host string) ? {
	// the case when the connection is dropped also has to be treated
	if proxy.has_conn == false || proxy.remote_host != host {
		proxy.reset_connection() ?
		proxy.open_connection(request, host) ?
	}
}

pub fn (mut proxy HttpProxy) open_connection(request &Request, host string) ? {
	$if trace_http_proxy ? {
		eprintln('<<< HttpProxy.open_connection  | host: $host')
	}

	proxy_headers := proxy.build_proxy_headers(request, host)

	mut client := proxy.create_tcp_layer() ?

	mut proxy_reader := io.new_buffered_reader(reader: client)

	client.write(proxy_headers.bytes()) ?

	mut proxy_response_status := proxy_reader.read_line() ?
	mut last_str := proxy_response_status

	for last_str != '' {
		last_str = proxy_reader.read_line() ?
	}

	_, status_code, status_msg := parse_status_line(proxy_response_status) ?

	if status_code < 200 || status_code >= 300 {
		return error('could not connect to proxy: $status_code $status_msg')
	}

	proxy_reader.free()

	proxy.conn = client
	proxy.remote_host = host
}

pub fn (mut proxy HttpProxy) reset_connection() ?bool {
	if proxy.has_conn == false {
		return false
	}

	proxy.conn.close() ?
	proxy.remote_host = ''
	proxy.has_conn = false

	return true
}
