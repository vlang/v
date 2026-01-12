module jsonrpc

import json


fn test_try_decode_encode() {
	obj_data := {'key': 'value'}
	mut enc_data := try_encode[map[string]string](obj_data)
	assert enc_data == json.encode(obj_data)
	assert try_decode[map[string]string](enc_data)! == obj_data
	
	str_data := 'string'
	enc_data = try_encode[string](str_data)
	assert enc_data == '"${str_data}"'
	assert try_decode[string](enc_data)! == str_data
	
	bool_data := true
	enc_data = try_encode[bool](bool_data)
	assert enc_data == bool_data.str()
	assert try_decode[bool](enc_data)! == bool_data
	
	int_data := 123
	enc_data = try_encode[int](int_data)
	assert enc_data == int_data.str()
	assert try_decode[int](enc_data)! == int_data
	
	null_data := null
	enc_data = try_encode[Null](null_data)
	assert enc_data == null_data.str()
	assert try_decode[Null](enc_data)! == null_data
	
	empty_data := empty
	enc_data = try_encode[Empty](empty)
	assert enc_data == empty_data.str()
	assert try_decode[Empty](enc_data)! == empty_data
}

fn test_request_obj_params() {
	id := "obj"
	method := "params." + id
	params := {"key": "value"}
	mut req := new_request(method, params, id)
	assert req.jsonrpc == version
	assert req.method == method
	assert req.params == json.encode(params)
	assert req.id == id

	enc_req := req.encode()
	assert enc_req == '{"jsonrpc":"${version}","method":"${method}","params":${json.encode(params)},"id":"${id}"}'

	assert req.decode_params[map[string]string]()! == params

	dec_req := decode_request(enc_req)!
	assert req.jsonrpc == dec_req.jsonrpc
	assert req.method == dec_req.method
	assert req.params == dec_req.params
	assert req.id == dec_req.id
}

fn test_request_notification() {
	id := ""
	method := "req.notif"
	params := "notif"
	mut req := new_request(method, params, id)
	assert req.jsonrpc == version
	assert req.method == method
	assert req.params == '"${params}"'
	assert req.id == id

	assert req.encode() == '{"jsonrpc":"${version}","method":"${method}","params":"${params}"}'
	assert req.decode_params[string]()! == params
}

fn test_request_empty_params() {
	id := ""
	method := "req.empty"
	params := empty
	mut req := new_request(method, params, id)
	assert req.jsonrpc == version
	assert req.method == method
	assert req.params == params.str()
	assert req.id == id

	assert req.encode() == '{"jsonrpc":"${version}","method":"${method}"}'
	assert req.decode_params[Empty]()! == params
}

fn test_request_batch() {
	n_id := ""
	n_method := "req.notif"
	n_params := "notif"
	mut notif := new_request(n_method, n_params, n_id)

	r_id := "obj"
	r_method := "params.obj"
	r_params := {"key": "value"}
	mut req := new_request(r_method, r_params, r_id)

	batch := [notif, req]
	enc_batch := batch.encode_batch()
	assert enc_batch == '[{"jsonrpc":"${version}","method":"${n_method}","params":"${n_params}"},{"jsonrpc":"${version}","method":"${r_method}","params":${json.encode(r_params)},"id":"${r_id}"}]'

	assert decode_batch_request(enc_batch)! == batch
}

fn test_response_obj_result() {
	id := "obj"
	result := {"key": "value"}
	mut resp := new_response(result, ResponseError{}, id)
	assert resp.jsonrpc == version
	assert resp.result == json.encode(result)
	assert resp.error == ResponseError{}
	assert resp.id == id

	enc_resp := resp.encode()
	assert enc_resp == '{"jsonrpc":"${version}","result":${json.encode(result)},"id":"${id}"}'

	assert resp.decode_result[map[string]string]()! == result

	dec_resp := decode_response(enc_resp)!
	assert resp.jsonrpc == dec_resp.jsonrpc
	assert resp.result == dec_resp.result
	assert resp.error == dec_resp.error
	assert resp.id == dec_resp.id
}

fn test_response_error() {
	id := "error"
	result := "should be null"
	err := parse_error
	mut resp := new_response(result, err, id)
	assert resp.jsonrpc == version
	assert resp.result == ""
	assert resp.error == parse_error
	assert resp.id == id

	enc_resp := resp.encode()
	assert enc_resp == '{"jsonrpc":"${version}","error":${json.encode(err)},"id":"${id}"}'

	assert resp.decode_result[Empty]()! == empty

	dec_resp := decode_response(enc_resp)!
	assert resp.jsonrpc == dec_resp.jsonrpc
	assert resp.result == dec_resp.result
	assert resp.error == dec_resp.error
	assert resp.id == dec_resp.id
}

fn test_response_null_id() {
	id := ""
	result := "should be null"
	err := parse_error
	mut resp := new_response(result, err, id)
	assert resp.jsonrpc == version
	assert resp.result == ""
	assert resp.error == parse_error
	assert resp.id == id

	enc_resp := resp.encode()
	assert enc_resp == '{"jsonrpc":"${version}","error":${json.encode(err)},"id":${null.str()}}'

	assert resp.decode_result[Empty]()! == empty

	dec_resp := decode_response(enc_resp)!
	assert resp.jsonrpc == dec_resp.jsonrpc
	assert resp.result == dec_resp.result
	assert resp.error == dec_resp.error
	assert resp.id == dec_resp.id
}

fn test_response_batch() {
	r_id := "obj"
	r_err := ResponseError{}
	r_result := {"key": "value"}
	mut r_resp := new_response(r_result, r_err, r_id)

	e_id := ""
	e_result := "should be null"
	e_err := parse_error
	mut e_resp := new_response(e_result, e_err, e_id)

	batch := [r_resp, e_resp]
	enc_batch := batch.encode_batch()
	assert enc_batch == '[{"jsonrpc":"${version}","result":${json.encode(r_result)},"id":"${r_id}"},{"jsonrpc":"${version}","error":${json.encode(e_err)},"id":${null.str()}}]'

	assert decode_batch_response(enc_batch)! == batch
}
