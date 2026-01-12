module jsonrpc

import json

pub const version = '2.0'

// ---- error helpers ----

pub struct ResponseError {
pub mut:
	code    int
	message string
	data    string
}

pub fn (err ResponseError) code() int { return err.code }
pub fn (err ResponseError) msg() string { return err.message }
pub fn (e ResponseError) err() IError { return IError(e) }

// ResponseErrorGeneratorParams & response_error are used by server.v :contentReference[oaicite:2]{index=2}
@[params]
pub struct ResponseErrorGeneratorParams {
	error IError @[required]
	data  string
}

@[inline]
pub fn response_error(params ResponseErrorGeneratorParams) ResponseError {
	return ResponseError{
		code: params.error.code()
		message: params.error.msg()
		data: params.data
	}
}

pub fn error_with_code(message string, code int) ResponseError {
	return ResponseError{ code: code, message: message, data: '' }
}

// JSON-RPC standard-ish errors :contentReference[oaicite:3]{index=3}

pub const parse_error          = error_with_code('Invalid JSON.', -32700)
pub const invalid_request      = error_with_code('Invalid request.', -32600)
pub const method_not_found     = error_with_code('Method not found.', -32601)
pub const invalid_params       = error_with_code('Invalid params', -32602)
pub const internal_error       = error_with_code('Internal error.', -32693)
pub const server_error_start     = error_with_code('Error occurred when starting server.', -32099)
pub const server_not_initialized = error_with_code('Server not initialized.', -32002)
pub const unknown_error          = error_with_code('Unknown error.', -32001)
pub const server_error_end       = error_with_code('Error occurred when stopping the server.', -32000)
pub const error_codes = [
		parse_error.code(), invalid_request.code(), method_not_found.code(), invalid_params.code(),
		internal_error.code(), server_error_start.code(), server_not_initialized.code(),
		server_error_end.code(), unknown_error.code(),
	]


// Null represents the null value in JSON.
pub struct Null {}
pub fn (n Null) str() string { return 'null' }
pub const null = Null{}

pub struct Empty {}
pub fn (e Empty) str() string { return '' }
pub const empty = Empty{}

// ---- request/response ----

// Request uses raw JSON strings for id and params in the old VLS code. :contentReference[oaicite:4]{index=4}
pub struct Request {
pub:
	jsonrpc string = version
	method  string
	params  string @[omitempty; raw] // raw JSON object/array/null
	id      string @[omitempty] // raw JSON (e.g. 1 or "abc") if empty => notification (no id field)
}

// new_request is the constructor for Request. ALWAYS use this to initialize new Request.
// if id is emply string ('') then the Request will be notification (no id field on encode).
// Pass jsonrpc.Empty{} as params to not generate params field on encode.
// jsonrpc.null can be used as params to set params field to json null on encode.
// Limitations: id is always string.
pub fn new_request[T] (method string, params T, id string) Request {
	return Request{
		method: method
		params: try_encode(params)
		id: id
	}
}

// (req Request) encode() returns json string representing Request.
// In returning string params field can be omited if in new_request was passed jsonrpc.Empty{} as params.
// In returning string id field can be omited if in new_request was passed empty string ('') as id.
pub fn (req Request) encode() string {
	params_payload := if req.params.len == 0 {
		''
	} else { 
		',"params":${req.params}'
	}
	id_payload := if req.id.len != 0 { ',"id":"${req.id}"' } else { '' }
	return '{"jsonrpc":"${version}","method":"${req.method}"${params_payload}${id_payload}}'
}

pub fn (reqs []Request) encode_batch() string {
	if reqs.len == 0 {return '[]'}
	mut s :='['+ reqs[0].encode()
	for req in reqs[1..] {
		s = s + ','+ req.encode()
	}
	return s + ']'
}

// decode_params tries to decode Request.params into provided type
pub fn (req Request) decode_params[T]() !T {
	return try_decode[T](req.params)
}

// decode_request decodes raw request into JSONRPC Request by reading after \r\n\r\n. :contentReference[oaicite:7]{index=7}
pub fn decode_request(raw string) !Request {
	json_payload := raw.all_after('\r\n\r\n')
	return json.decode(Request, json_payload) or { return err }
}

// decode_batch_request decodes raw batch request into []jsonrpc.Request by reading after \r\n\r\n.
pub fn decode_batch_request(raw string) ![]Request {
	json_payload := raw.all_after('\r\n\r\n')
	return json.decode([]Request, json_payload) or { return err }
}

pub struct Response {
pub:
	jsonrpc string = version
	result  string @[raw]
	error   ResponseError
	id      string
}

pub fn new_response[T] (result T, error ResponseError, id string) Response {
	return Response{
		result: if error.code != 0 { ''	} else { try_encode(result)	}
		error: error
		id: id
	}
}

pub fn (resp Response) encode() string {
	mut s := '{"jsonrpc":"${version}"'
	if resp.error.code != 0 {
		s = s + ',"error":' + json.encode(resp.error)
	} else {
		s = s + ',"result":' + resp.result
	}
	s = s + ',"id":'
	if resp.id.len == 0 {
		s = s + null.str()
	} else {
		s = s + '"${resp.id}"'
	}
	return s + '}'
}

pub fn (resps []Response) encode_batch() string {
	if resps.len == 0 {return '[]'}
	mut s :='['+ resps[0].encode()
	for resp in resps[1..] {
		s = s + ','+ resp.encode()
	}
	return s + ']'
}

pub fn (resp Response) decode_result[T]() !T {
	return try_decode[T](resp.result)
}

pub fn decode_response(raw string) !Response {
	json_payload := raw.all_after('\r\n\r\n')
	return json.decode(Response, json_payload) or { return err }
}

pub fn decode_batch_response(raw string) ![]Response {
	json_payload := raw.all_after('\r\n\r\n')
	return json.decode([]Response, json_payload) or { return err }
}

// try_encode tries to encode passed value to json object, array or primitive
// currently only for internal use
fn try_encode[T](data T) string {
	return $if data is string {
		'"${data}"'
	} $else $if data is bool {
		data.str()
	} $else $if data is int {
		data.str()
	} $else $if data is Null {
		data.str()
	} $else $if data is Empty {
		data.str()
	} $else {
		json.encode(data)
	}
}

fn try_decode[T](s string) !T {
	$if T is string {
		if s[0] == `"` && s[s.len-1] == `"` { return s.find_between('"', '"\0') }
		return error('Could not decode data=${s} into type string')
	} $else $if T is bool {
		if s == 'true' { return true }
		if s == 'false' { return false }
		return error('Could not decode data=${s} into type bool')
	} $else $if T is int {
		res := s.int()
		if res == 0 && s != '0' { return error('Could not decode data=${s} into type int') }
		return res
	} $else $if T is Null {
		if s != null.str() { return error('Could not decode data=${s} into type bool') }
		return null
	} $else $if T is Empty {
		if s.len == 0 { return Empty{} }
		return error('Params not empty: data=${s}')
	} $else {
		return json.decode(T, s) or { return err }
	}
}