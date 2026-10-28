module veb

import json2 as json
import math
import net.http

struct JsonencUser {
	id     int
	name   string
	email  string
	active bool
	score  f64
}

struct JsonencNested {
	user JsonencUser
	tags []string
	meta map[string]string
}

fn test_json_encode_into_matches_encode_for_structs() {
	val := JsonencUser{
		id:     7
		name:   'ada "l" \\ test'
		email:  'ada@example.com'
		active: true
		score:  0.1 + 0.2
	}
	mut buf := []u8{cap: 2048}
	json_encode_into(val, mut buf)
	assert buf.bytestr() == json.encode(val)
}

fn test_json_encode_into_matches_encode_for_nested_arrays_maps() {
	val := JsonencNested{
		user: JsonencUser{
			id:     1
			name:   'grace'
			email:  'grace@example.com'
			active: false
			score:  1.5
		}
		tags: ['a', 'b"c', 'ünïcödé']
		meta: {
			'role':  'admin'
			'empty': ''
		}
	}
	mut buf := []u8{cap: 2048}
	json_encode_into(val, mut buf)
	assert buf.bytestr() == json.encode(val)

	mut listbuf := []u8{cap: 4096}
	users := [
		JsonencUser{ id: 1, name: 'a', email: 'a@x.io', active: true, score: 2.0 },
		JsonencUser{
			id:     2
			name:   'b'
			email:  'b@x.io'
			active: false
			score:  1e-7
		},
		JsonencUser{
			id:     3
			name:   'c'
			email:  'c@x.io'
			active: true
			score:  1e21
		},
	]
	json_encode_into(users, mut listbuf)
	assert listbuf.bytestr() == json.encode(users)
}

fn test_json_encode_into_matches_encode_for_float_edge_cases() {
	// Shortest round-trip, compact exponents, `.0` trimming, NaN/Inf to null:
	// all inherited from json2, pinned here so a future fast path cannot drift.
	for f in [0.0, 1.5, 0.1 + 0.2, 1e-7, 1e21, -2.5e-10, 100.0] {
		mut buf := []u8{cap: 64}
		json_encode_into(f, mut buf)
		assert buf.bytestr() == json.encode(f), 'float ${f}'
	}
	mut nanbuf := []u8{cap: 16}
	json_encode_into(math.nan(), mut nanbuf)
	assert nanbuf.bytestr() == 'null'
}

fn test_json_encode_into_matches_encode_for_error_envelope() {
	// This is the exact shape `Context.error_with` sends; any fast path must
	// reproduce it byte-for-byte or API clients see a different contract.
	msg := 'boom "quoted" \\ backslash'
	mut buf := []u8{cap: 128}
	json_encode_into({
		'error': msg
	}, mut buf)
	assert buf.bytestr() == json.encode({
		'error': msg
	})
}

fn test_json_encode_into_reuses_buffer_capacity() {
	mut buf := []u8{cap: 2048}
	before := buf.cap
	val := JsonencUser{
		id:     9
		name:   'reuse'
		email:  'r@x.io'
		active: true
		score:  3.25
	}
	for _ in 0 .. 10 {
		buf.clear()
		json_encode_into(val, mut buf)
	}
	assert buf.cap == before
	assert buf.bytestr() == json.encode(val)
}

fn jsonenc_ctx() Context {
	// Same shape as the helper in context_response_test.v (helpers are not
	// shared across _test.v programs): no socket, response ready to fill.
	return Context{
		req:   http.Request{
			method: .get
			url:    '/'
			header: http.new_header()
		}
		query: {}
		form:  {}
		files: {}
		res:   http.Response{
			header: http.new_header()
		}
	}
}

fn test_json_buf_matches_json_response() {
	mut buf := []u8{cap: 2048}
	val := JsonencUser{
		id:     42
		name:   'lin'
		email:  'lin@x.io'
		active: false
		score:  9.75
	}
	mut ctx := jsonenc_ctx()
	ctx.json_buf(val, mut buf)
	assert ctx.res.status_code == int(http.Status.ok)
	assert ctx.res.body == json.encode(val)
	got := ctx.res.header.get(.content_type) or { '' }
	assert got == 'application/json'
	// The body owns its copy: reusing the buffer must not corrupt it.
	buf.clear()
	json_encode_into('other', mut buf)
	assert ctx.res.body == json.encode(val)
}

fn test_json_buf_matches_error_with_envelope() {
	mut ctx := jsonenc_ctx()
	ctx.res.set_status(.internal_server_error)
	mut buf := []u8{cap: 128}
	ctx.json_buf({
		'error': 'boom'
	}, mut buf)
	assert ctx.res.status_code == int(http.Status.internal_server_error)
	assert ctx.res.body == '{"error":"boom"}'
}
