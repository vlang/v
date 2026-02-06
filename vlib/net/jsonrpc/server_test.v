module jsonrpc

import strings

struct StringRW {
mut:
	buf strings.Builder = strings.new_builder(4096)
}

fn (mut s StringRW) read(mut buf []u8) !int {
	len := s.buf.len
	buf = s.buf.str().bytes()
	s.buf = strings.new_builder(4096)
	return len
}

fn (mut s StringRW) write(buf []u8) !int {
	return s.buf.write(buf)
}

struct KVItem {
	key   string
	value string
}

fn handle_test(req &Request, mut wr ResponseWriter) {
	p := req.decode_params[KVItem]() or {
		wr.write_error(invalid_params)
		return
	}

	wr.write(p)
}

fn test_server_request_response() {
	mut stream := StringRW{}
	mut srv := new_server(ServerConfig{
		stream:  stream
		handler: handle_test
	})

	id := 'req'
	method := 'kv.item'
	params := KVItem{
		key:   'foo'
		value: 'bar'
	}
	stream.write(new_request(method, params, id).encode().bytes())!

	srv.respond()!

	mut enc_resp := []u8{len: 4096}
	stream.read(mut enc_resp)!
	resp := decode_response(enc_resp.bytestr())!

	assert resp.jsonrpc == version
	assert resp.decode_result[KVItem]()! == params
	assert resp.error == ResponseError{}
	assert resp.id == id
}

fn test_server_router_request_response() {
	mut r := Router{}
	method := 'kv.item'
	r.register(method, handle_test)
	mut stream := StringRW{}
	mut srv := new_server(ServerConfig{
		stream:  stream
		handler: r.handle_jsonrpc
	})

	id := 'req'
	params := KVItem{
		key:   'foo'
		value: 'bar'
	}
	stream.write(new_request(method, params, id).encode().bytes())!

	srv.respond()!

	mut enc_resp := []u8{len: 4096}
	stream.read(mut enc_resp)!
	mut resp := decode_response(enc_resp.bytestr())!

	assert resp.jsonrpc == version
	assert resp.decode_result[KVItem]()! == params
	assert resp.error == ResponseError{}
	assert resp.id == id

	stream.write(new_request('unknown', params, id).encode().bytes())!

	srv.respond()!

	enc_resp = []u8{len: 4096}
	stream.read(mut enc_resp)!
	resp = decode_response(enc_resp.bytestr())!

	assert resp.jsonrpc == version
	assert resp.decode_result[Empty]()! == empty
	assert resp.error == method_not_found
	assert resp.id == id
}
