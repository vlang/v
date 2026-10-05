// The client half of the example: every RPC shape of kv.proto against kvd.
//
//   v run examples/grpc/server.v     # in one terminal
//   v run examples/grpc/client.v     # in another
module main

import net.grpc
import os
import time
import kv

// The bundled certificate is issued to `localhost`, so the client connects by
// name: a raw IP would fail hostname verification even with the right root.
const server_addr = 'localhost:50051'

// main exercises unary, server-streaming, client-streaming and the error path,
// so it doubles as a smoke test of a running kvd.
fn main() {
	mut client := grpc.Client{
		// gRPC is HTTP/2-only and net.http only negotiates h2 for https, so this
		// must be a TLS URL. grpc.Client opts into enable_http2 itself.
		base_url: 'https://${server_addr}'
		// The bundled certificate is self-signed, so trusting it as its own root
		// is how this client pins it. Point `verify` at your CA in real use.
		// @FILE anchors the path at compile time, so the executable can live
		// anywhere.
		verify:   os.join_path(os.dir(@FILE), 'cert', 'server.crt')
	}

	unary(&client)
	server_streaming(&client)
	client_streaming(&client)
	with_a_deadline(&client)
	error_path(&client)
}

// unary calls Get and Put: one request message for one response message.
fn unary(client &grpc.Client) {
	println('--- unary ---')
	put := kv.PutRequest{
		key:   'answer'
		value: '42'.bytes()
	}
	put_body := put.encode() or {
		eprintln('put: could not encode the request: ${err}')
		return
	}
	put_reply := client.unary(kv.kv_method_put, put_body) or {
		eprintln('put failed: ${err}')
		return
	}
	replaced := kv.decode_put_response(put_reply.payload) or {
		eprintln('put reply undecodable: ${err}')
		return
	}
	println('put answer=42 replaced=${replaced}')

	get := kv.GetRequest{
		key: 'answer'
	}
	get_body := get.encode() or {
		eprintln('get: could not encode the request: ${err}')
		return
	}
	get_reply := client.unary(kv.kv_method_get, get_body) or {
		eprintln('get failed: ${err}')
		return
	}
	found := kv.decode_get_response(get_reply.payload) or {
		eprintln('get reply undecodable: ${err}')
		return
	}
	println('get answer -> found=${found.found} value=${found.value.bytestr()}')

	missing_req := kv.GetRequest{
		key: 'no-such-key'
	}
	missing_body := missing_req.encode() or {
		eprintln('missing: could not encode the request: ${err}')
		return
	}
	missing_reply := client.unary(kv.kv_method_get, missing_body) or {
		eprintln('get failed: ${err}')
		return
	}
	// A missing key is a normal answer, not an error: the service replies with
	// found=false rather than a status.
	missing := kv.decode_get_response(missing_reply.payload) or { return }
	println('get no-such-key -> found=${missing.found}')
}

// server_streaming calls Scan: one request, many responses. The whole stream is
// buffered by the transport, so this is a single POST internally.
fn server_streaming(client &grpc.Client) {
	println('--- server streaming ---')
	for key in ['alpha', 'beta'] {
		req := kv.PutRequest{
			key:   key
			value: key.bytes()
		}
		put_body := req.encode() or {
			eprintln('put: could not encode the request: ${err}')
			return
		}
		client.unary(kv.kv_method_put, put_body) or {
			eprintln('put failed: ${err}')
			return
		}
	}
	scan := kv.GetRequest{}
	scan_body := scan.encode() or {
		eprintln('scan: could not encode the request: ${err}')
		return
	}
	reply := client.server_stream(kv.kv_method_scan, scan_body) or {
		eprintln('scan failed: ${err}')
		return
	}
	for raw in reply.payloads {
		entry := kv.decode_get_response(raw) or { continue }
		println('scan -> ${entry.value.bytestr()}')
	}
	// Trailing metadata the service set with ctx.set_trailer, folded into the
	// response header set by the HTTP/2 layer.
	if reply.metadata['x-kv-scanned'].len > 0 {
		println('scan reported ${reply.metadata['x-kv-scanned'][0]} entries')
	}
}

// client_streaming calls PutMany: many requests, one response.
fn client_streaming(client &grpc.Client) {
	println('--- client streaming ---')
	msgs := [
		kv.PutRequest{
			key:   'k1'
			value: 'v1'.bytes()
		},
		kv.PutRequest{
			key:   'k2'
			value: 'v2'.bytes()
		},
		kv.PutRequest{
			key:   'k3'
			value: 'v3'.bytes()
		},
	]
	mut framed := [][]u8{cap: msgs.len}
	for m in msgs {
		framed << m.encode() or {
			eprintln('put_many: could not encode a request: ${err}')
			return
		}
	}
	reply := client.client_stream(kv.kv_method_put_many, framed) or {
		eprintln('put_many failed: ${err}')
		return
	}
	written := kv.decode_put_many_response(reply.payload) or { return }
	println('put_many -> written=${written.written}')
}

// error_path shows that a handler's StatusError crosses the wire as a typed
// gRPC status rather than as a transport failure.
fn error_path(client &grpc.Client) {
	println('--- error path ---')
	bad := kv.PutRequest{
		key: '' // the service rejects this with invalid_argument
	}
	bad_body := bad.encode() or {
		eprintln('could not encode the request: ${err}')
		return
	}
	if _ := client.unary(kv.kv_method_put, bad_body) {
		println('unexpected success for an empty key')
	} else {
		if err is grpc.StatusError {
			println('put with an empty key -> ${err.status.code}: ${err.status.message}')
		} else {
			println('unexpected transport error: ${err}')
		}
	}

	unknown_req := kv.GetRequest{}
	unknown_body := unknown_req.encode() or {
		eprintln('could not encode the request: ${err}')
		return
	}
	if _ := client.unary('/kv.KV/NoSuchMethod', unknown_body) {
		println('unexpected success for an unknown method')
	} else {
		if err is grpc.StatusError {
			println('unknown method -> ${err.status.code}')
		}
	}
}

// with_a_deadline shows the per-call options: a deadline and request metadata,
// both applied functionally rather than through a config struct.
fn with_a_deadline(client &grpc.Client) {
	println('--- deadline and metadata ---')
	get := kv.GetRequest{
		key: 'answer'
	}
	get_body := get.encode() or {
		eprintln('could not encode the request: ${err}')
		return
	}
	reply := client.unary(kv.kv_method_get, get_body, grpc.timeout(2 * time.second),
		grpc.header('authorization', 'Bearer demo')) or {
		eprintln('call with a deadline failed: ${err}')
		return
	}
	value := kv.decode_get_response(reply.payload) or { return }
	println('got ${value.value.bytestr()} within the 2s deadline')
}
