module http

import net

pub struct HttpServer {
	handler fn(req Request, res Response) Response
	pub mut: 
	sock net.Socket

}

// create an http server, beware: the handler callback runs on a different system thread.
pub fn create_server(handler fn(req Request, res Response) Response) ?HttpServer {
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
// handle connections, this runs in a different system thread.
fn (s HttpServer) handle_conn(conn net.Socket) {
	conn.write('HTTP/1.1 404 Not Found\r\nContent-Type: text/plain\r\n\r\n404 Not Found') or {
		panic(err)
	}
	conn.close() or {
		panic(err)
	}
}