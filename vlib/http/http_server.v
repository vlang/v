module http

import net

pub struct HttpServer {
	handler fn(req ServerRequest, res Response) Response
	pub mut: 
	sock net.Socket

}

pub struct ServerRequest {
	dummy string
}


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

// handle connections, this runs in a different system thread.
fn (s HttpServer) handle_conn(conn net.Socket) ?bool {
	mut headers := map[string]string
	headers['User-Agent'] = 'vlang'
	headers['Content-Type'] = 'text/plain'
	res := Response {
		text: 'Not Found',
		status_code: 404,
		headers: headers
	}
	serialized_headers := serialize_headers(res.headers)
	req := ServerRequest {
		dummy: 'test'
	}
	// new_res := s.handler(req, res) V bug preventing this from working
	new_res := res
	conn.write('HTTP/1.1 $new_res.status_code\r\n$serialized_headers\r\n$new_res.text') or {
		return error(err)
	}
	conn.close() or {
		return error(err)
	}
	return true
}