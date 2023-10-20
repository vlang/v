module http

import net.urllib
import encoding.base64
import net

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

pub fn new_http_proxy(raw_url string) !HttpProxy {
	mut url := urllib.parse(raw_url) or { return error('malformed proxy url') }
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
		username: url.user.username
		password: url.user.password
		host: host
		hostname: url.hostname()
		port: port
		url: str_url
	}
}

fn (proxy HttpProxy) build_proxy_headers(request &Request, host string, path string) string {
	ua := request.user_agent
	mut uheaders := []string{}

	uheaders << 'Host: ${host}\r\n'
	uheaders << 'User-Agent: ${ua}\r\n'
	uheaders << 'Upgrade-Insecure-Requests: 1\r\n'
	// uheaders << 'Proxy-Connection: Keep-Alive\r\n'

	if proxy.username != '' {
		mut authinfo := ''

		authinfo += proxy.username
		if proxy.password != '' {
			authinfo += ':${proxy.password}'
		}

		encoded_authinfo := base64.encode(authinfo.bytes())

		uheaders << 'Proxy-Authorization: Basic ${encoded_authinfo}\r\n'
	}

	version := Version.v1_1

	// we always make requests on http scheme
	url := 'http://${host}${path}'

	return '${request.method} ${url} ${version}\r\n' + uheaders.join('') + '\r\n'
}

fn (proxy &HttpProxy) http_do(host string, method Method, path string, req &Request) !Response {
	host_name, _ := net.split_address(host)!
	s := proxy.build_proxy_headers(req, host_name, path)
	mut client := net.dial_tcp(proxy.host)!
	client.set_read_timeout(req.read_timeout)
	client.set_write_timeout(req.write_timeout)
	// TODO this really needs to be exposed somehow
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
