module http

import net
import strings

fn (req &Request) http_do(port int, method, host_name, path string) ?Response {
	bufsize := 512
	rbuffer := [512]byte
	mut sb := strings.new_builder(100)
	s := req.build_request_headers(method, host_name, path)
	client := net.dial(host_name, port) or {
		return error(err)
	}
	client.send(s.str, s.len) or {
	}
	for {
		readbytes := client.crecv(rbuffer, bufsize)
		if readbytes < 0 {
			return error('http.request.http_do: error reading response. readbytes=$readbytes')
		}
		if readbytes == 0 {
			break
		}
		sb.write(tos(rbuffer, readbytes))
	}
	client.close() or {
	}
	return parse_response(sb.str())
}

