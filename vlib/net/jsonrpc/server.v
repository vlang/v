module jsonrpc

import json
import strings
import io

pub struct ServerConfig {
pub mut:
	stream       io.ReaderWriter
	handler      Handler @[required]
	interceptors Interceptors
}

// Server represents a JSONRPC server that sends/receives data
// from a stream (an io.ReaderWriter) and uses Content-Length framing. :contentReference[oaicite:6]{index=6}
@[heap]
pub struct Server {
mut:
	stream       io.ReaderWriter
	handler      Handler @[required]
	interceptors Interceptors
}

// new_server creates new `jsonrpc.Server` with `stream` to read/write, 
// the `jsonrpc.Handler` to handle Requests/Responses and `interceptors`
pub fn new_server(cfg ServerConfig) Server {
	return Server{
		stream:       cfg.stream
		handler:      cfg.handler
		interceptors: cfg.interceptors
	}
}

// respond reads bytes from stream, pass them to the `interceptors.encoded_request`,
// tries to decode into `jsonrpc.Request` and pass to `interceptors.request`
// and on fail it responds with `jsonrpc.parse_error` after that it calls handlers
// (batch requests are handled automatically as well as writing batch response)
// and passes recieved `jsonrpc.Response` into `interceptors.response` and the
// last step is to encode `jsonrpc.Response`, pass it into `interceptors.encoded_response`
// and write to stream
pub fn (mut s Server) respond() ! {
	mut rw := s.writer()
	mut rx := []u8{len: 4096}
	bytes_read := s.stream.read(mut rx) or {
		if err is io.Eof {
			return
		}
		return err
	}

	if bytes_read == 0 {
		return
	}

	intercept_encoded_request(s.interceptors.encoded_request, rx) or {
		rw.write_error(response_error(error: err))
		return err
	}

	req_str := rx.bytestr()

	mut req_batch := []Request{}
	match req_str[0].ascii_str() {
		'[' {
			req_batch = decode_batch_request(req_str) or {
				rw.write_error(response_error(error: parse_error))
				return err
			}
			rw.start_batch()
		}
		'{' {
			req := decode_request(req_str) or {
				rw.write_error(response_error(error: parse_error))
				return err
			}
			req_batch.prepend(req)
		}
		else {
			rw.write_error(response_error(error: parse_error))
			return parse_error
		}
	}

	for rq in req_batch {
		rw.req_id = rq.id

		intercept_request(s.interceptors.request, &rq) or {
			rw.write_error(response_error(error: err))
			return err
		}

		s.handler(&rq, mut rw)
	}

	if req_batch.len > 1 {
		rw.close_batch()
	}
}

fn (s &Server) writer() &ResponseWriter {
	return &ResponseWriter{
		writer: s.stream
		sb:     strings.new_builder(4096)
		server: s
	}
}

// start `Server` loop to operate on `stream` passed into constructor
// it calls `Server.respond()` method in loop
pub fn (mut s Server) start() {
	for {
		s.respond() or {
			if err is io.Eof {
				return
			}
		}
	}
}

// Handler is the function called when `jsonrpc.Request` is
// decoded and `jsonrpc.Response` is required which is written
// into `jsonrpc.ResponseWriter`. Before returning from Handler
// either `wr.write()` or `wr.write_error()` must be called
// or the stream will stuck awaiting writing `jsonrpc.Response`
pub type Handler = fn (req &Request, mut wr ResponseWriter)

// Router is simple map of method names and their `Handler`s
pub struct Router {
mut:
	methods map[string]Handler
}

// handle_jsonrpc must be passed into `Server` handler field to operate
// it simply tries to invoke registered methods and if none valid found
// writes `jsonrpc.method_not_found` error into `jsonrpc.ResponseWriter`
pub fn (r Router) handle_jsonrpc(req &Request, mut wr ResponseWriter) {
	if h := r.methods[req.method] {
		h(req, mut wr)
		return
	}

	wr.write_error(method_not_found)
}

// register `handler` to operate when `method` found in incoming `jsonrpc.Request`
pub fn (mut r Router) register(method string, handler Handler) bool {
	if method in r.methods {
		return false
	}

	r.methods[method] = handler
	return true
}

pub struct ResponseWriter {
mut:
	sb       strings.Builder
	is_batch bool
	server   &Server
pub mut:
	req_id string
	writer io.ReaderWriter
}

fn (mut rw ResponseWriter) start_batch() {
	rw.is_batch = true
	rw.sb.write_string('[')
}

fn (mut rw ResponseWriter) close_batch() {
	rw.is_batch = false
	rw.sb.go_back(2)
	rw.sb.write_string(']')
	rw.close()
}

fn (mut rw ResponseWriter) close() {
	intercept_encoded_response(rw.server.interceptors.encoded_response, rw.sb)
	rw.writer.write(rw.sb) or {}
	rw.sb.go_back_to(0)
}

// write payload into `jsonrpc.Response.result`.
// call when need to send data in response
pub fn (mut rw ResponseWriter) write[T](payload T) {
	final_resp := Response{
		id:     rw.req_id
		result: json.encode(payload)
	}

	intercept_response(rw.server.interceptors.response, final_resp)

	if rw.req_id.len == 0 {
		return
	}

	rw.sb.write_string(final_resp.encode())

	if rw.is_batch == true {
		rw.sb.write_string(', ')
		return
	}
	rw.close()
}

// write_empty writes `jsonrpc.null` as response
pub fn (mut rw ResponseWriter) write_empty() {
	rw.write[Null](null)
}

// write_error into the `jsonrpc.Response` of current request
pub fn (mut rw ResponseWriter) write_error(err IError) {
	mut res_err := err
	if err !is ResponseError {
		if err.code() !in error_codes {
			res_err = response_error(error: unknown_error)
		} else {
			res_err = response_error(error: err)
		}
	}

	final_resp := Response{
		id:    rw.req_id
		error: res_err as ResponseError
	}

	intercept_response(rw.server.interceptors.response, final_resp)

	rw.sb.write_string(final_resp.encode())
	if rw.is_batch {
		rw.sb.write_string(', ')
		return
	}
	rw.close()
}
