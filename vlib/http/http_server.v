module http

import net

pub struct HttpServer {
	handler fn(req ServerRequest, res Response) Response
	pub mut: 
	sock net.Socket

}

pub struct ServerRequest {
	path string
	method string
	headers map[string]string
	text ?string // Only on POST
}

const (
	allowed_methods = ['GET', 'POST']
	default_headers = {
		'User-Agent': 'vlang',
		'Content-Type': 'text/plain'
	}
)


// create an http server, beware: the handler callback runs on a different system thread.
pub fn create_server(handler fn(req ServerRequest, res Response) Response) ?HttpServer {
	sock := net.new_socket(C.AF_INET, C.SOCK_STREAM, 0) or {
		return error(err)
	}
	return HttpServer {
		handler,
		sock
	}
}
// listen on the specified port
pub fn (s HttpServer) listen(port int) ?int {
	s.sock.bind(port) or {
		return error(err)
	}
	s.sock.listen() or {
		return error(err)
	}
	
	for {
		conn := s.sock.accept() or {
			return error(err)
		}
		go s.handle_conn(conn)
	}

	return port
}

pub fn (s HttpServer) free() ?bool {
	s.sock.close() or {
		return error(err)
	}
	return true
}

fn get_response_status_message(code int) string {
	status := match code {
		100 { 'Continue' }
		101 { 'Switching Protocols' }
		200 { 'OK' }
		201 { 'Created' }
		202 { 'Accepted' }
		203 { 'Non-Authoritive Information' }
		204 { 'No Content' }
		205 { 'Reset Content' }
		206 { 'Partial Content' }
		300 { 'Multiple Choices' }
		301 { 'Moved Permanently' }
		400 { 'Bad Request' }
		401 { 'Unauthorized' }
		403 { 'Forbidden' }
		404 { 'Not Found' }
		405 { 'Method Not Allowed' }
		408 { 'Request Timeout' }
		500 { 'Internal Server Error' }
		501 { 'Not Implemented' }
		502 { 'Bad Gateway' }
		else { 'Internal Server Error' }
	}

	return status
}
fn get_status_res(code int) Response {
	return Response {
		text: get_response_status_message(code),
		status_code: code,
		headers: default_headers
	}
}

fn (r Response) send(conn net.Socket) ?bool {
	serialized_headers := serialize_headers(r.headers)
	conn.write('HTTP/1.1 $r.status_code\r\n$serialized_headers\r\n$r.text') or {
		return error(err)
	}
	conn.close() or {
		return error(err)
	}
	return true
}
// handle connections, this runs in a different system thread.
fn (s HttpServer) handle_conn(conn net.Socket) ?bool {
	res := get_status_res(404)

	meta_line := conn.read_line().split(' ')
	if (meta_line.len != 3) {
		get_status_res(400).send(conn) or {
			return error('couldnt send response')
		}
		return error('invalid response')
	}
	req := ServerRequest {
		method: meta_line[0],
		path: meta_line[1],
		headers: default_headers
	}
	// TODO: impl allowed_methods
	handler := s.handler
	new_res := handler(req, res)
	new_res.send(conn) or {
		return error('couldnt send response')
	}
	return true
}