module jsonrpc

import io

pub struct ClientConfig {
pub mut:
	stream io.ReaderWriter
	interceptors Interceptors
}

pub struct Client {
mut:
	stream io.ReaderWriter
	interceptors Interceptors
}

pub fn new_client(cfg ClientConfig) Client {
	return Client{
		stream: cfg.stream
		interceptors: cfg.interceptors
	}
}

// notify sends JSON-RPC 2.0 Notification and returns without waiting for `jsonrpc.Response`
pub fn (mut c Client) notify[T](method string, params T) ! {
	mut req := new_request(method, params, "")
	intercept_request(c.interceptors.request, &req) or {
		return err
	}

	mut enc_req := req.encode().bytes()
	intercept_encoded_request(c.interceptors.encoded_request, enc_req) or {
		return err
	}
	
	c.stream.write(enc_req) or { return err }
}

// request new `jsonrpc.Request` and return `jsonrpc.Response`
pub fn (mut c Client) request[T](method string, params T, id string) !Response {
	mut req := new_request(method, params, id)
	intercept_request(c.interceptors.request, &req) or {
		return err
	}

	mut enc_req := req.encode().bytes()
	intercept_encoded_request(c.interceptors.encoded_request, enc_req) or {
		return err
	}
	
	c.stream.write(enc_req) or { return err }

	mut enc_resp := []u8{len: 4096}
	c.stream.read(mut enc_resp) or {
		return err
	}

	intercept_encoded_response(c.interceptors.encoded_response, enc_resp)

	resp := decode_response(enc_resp.bytestr()) or {
		return err
	}

	intercept_response(c.interceptors.response, resp)

	return resp
}

// batch sends batch of `jsonrpc.Request` and returns batch of`jsonrpc.Response`
pub fn (mut c Client) batch(reqs []Request) ![]Response {
	mut reqs_str := "["
	for req in reqs {
		intercept_request(c.interceptors.request, &req) or {
			return err
		}
		reqs_str = reqs_str + req.encode() + ", "
	}
	
	reqs_str = reqs_str.all_before_last(", ") + "]"
	enc_reqs := reqs_str.bytes()
	intercept_encoded_request(c.interceptors.encoded_request, enc_reqs) or {
		return err
	}
	
	c.stream.write(enc_reqs) or { return err }

	mut enc_resp := []u8{len: 4096}
	c.stream.read(mut enc_resp) or {
		return err
	}

	intercept_encoded_response(c.interceptors.encoded_response, enc_resp)

	mut resps := decode_batch_response(enc_resp.bytestr()) or {
		return err
	}

	for mut resp in resps {
		intercept_response(c.interceptors.response, &resp)
	}

	return resps
}