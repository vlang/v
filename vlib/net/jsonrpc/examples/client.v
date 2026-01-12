module main

import json
import jsonrpc
import net

fn main() {
	addr := '127.0.0.1:42228'
	mut stream := net.dial_tcp(addr)!
	mut log_inter := jsonrpc.LoggingInterceptor{}
	mut inters := jsonrpc.Interceptors{
		event:            [log_inter.on_event]
		encoded_request:  [log_inter.on_encoded_request]
		request:          [log_inter.on_request]
		response:         [log_inter.on_response]
		encoded_response: [log_inter.on_encoded_response]
	}

	mut c := jsonrpc.new_client(jsonrpc.ClientConfig{
		stream:       stream
		interceptors: inters
	})

	println('TCP JSON-RPC client on ${addr}')

	d1 := c.request('kv.delete', {
		'key': 'foo'
	}, 'kv.delete')!
	println('RESULT: ${d1}')

	res := c.batch([
		jsonrpc.new_request('kv.create', {
			'key':   'foo'
			'value': 'bar'
		}, 'kv.create'),
		jsonrpc.new_request('kv.create', {
			'key':   'bar'
			'value': 'foo'
		}, 'kv.create'),
	])!
	println('RESULT: ${res}')

	c.notify('kv.create', {
		'key':   'bazz'
		'value': 'barr'
	})!
}
