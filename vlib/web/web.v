module web

import net
import os

// until embedded structs are supported
// we must reconstruct the net.Socket struct
// every time we want to call one of its methods
struct Server {
	sockfd int
	family int
	_type int
	proto int
	port int
}

// create server
pub fn server(port int) Server {
	socket := net.socket(AF_INET, SOCK_STREAM, 0)
	opt := 1
	socket.setsockopt(SOL_SOCKET, SO_REUSEADDR | SO_REUSEPORT, &opt)
	s := Server {
		sockfd: socket.sockfd
		family: socket.family
		_type: socket._type
		proto: socket.proto
		port: port
	}
	return s
}

// serve server forever
pub fn (s Server) serve() {
	socket := net.Socket {
		sockfd: s.sockfd
		family: s.family
		_type: s._type
		proto: s.proto
	}
	socket.bind(s.port)
	socket.listen()

	dir := os.getwd() + '/'

	for {
		connection := socket.accept()

		if os.fork() == 0 {
			request := tos(connection.recv(65535), 65535)

			data := request.substr(0, request.index('\r\n')).split(' ')
			method := data[0]
			uri := data[1]
			version := data[2]

			// TODO: build request struct
			// and do stuff with headers

			switch method {
			case 'GET':
				mut path := dir
				if uri == '/' {
					path = dir + 'index.html'
				}
				else {
					path = dir + uri
				}

				contents := os.read_file(path) or {
					return
				}
				length := contents.len
				mt := mimetype(path)

				// TODO: build response struct
				// and set other headers
				mut response := '$version 200 OK\r\n'
				response += 'Content-Type: $mt\r\n'
				response += 'Content-Length: $length\r\n\r\n'
				response += contents
				response_raw := response.cstr()
				connection.send(response_raw, response.len)
			}

			connection.close()
			exit(0)
		}
	}
}

pub fn mimetype(filename string) string {
	ext := os.ext(filename)
	mut mt := 'application/octet-stream'
	// TODO: use map
	switch ext {
	case '.css':
		mt = 'text/css'
	case '.gif':
		mt = 'image/gif'
	case '.htm':
		mt = 'text/html'
	case '.html':
		mt = 'text/html'
	case '.ico':
		mt = 'image/vnd.microsoft.icon'
	case '.jpeg':
		mt = 'image/jpeg'
	case '.jpg':
		mt = 'image/jpeg'
	case '.js':
		mt = 'text/javascript'
	case '.json':
		mt = 'application/json'
	case '.png':
		mt = 'image/png'
	case '.pdf':
		mt = 'application/pdf'
	case '.txt':
		mt = 'text/plain'
	case '.xml':
		mt = 'application/xml'
	}
	return mt
}
