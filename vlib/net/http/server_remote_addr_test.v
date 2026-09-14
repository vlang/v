// Tests for Request.remote_addr, the address of the peer that sent a request
// (the equivalent of Go's http.Request.RemoteAddr), and for the legacy
// `Remote-Addr` header the V server mirrors it into.
module http

import net
import time

const ratimeout = 500 * time.millisecond

struct RemoteAddrHandler {}

fn (mut h RemoteAddrHandler) handle(req Request) Response {
	return Response{
		status_code: 200
		body:        '${req.remote_addr}|${req.remote_ip()}|${req.header.get_custom('Remote-Addr') or { '<none>' }}'
	}
}

fn remote_addr_server() &Server {
	return &Server{
		accept_timeout:       ratimeout
		handler:              RemoteAddrHandler{}
		addr:                 ''
		show_startup_message: false
	}
}

// raw_request writes `head` verbatim to a fresh connection to `addr` and
// returns the response body. Going through a raw socket rather than
// http.fetch is what lets a test send header names the client would
// normally never produce.
fn raw_request(addr string, head string) !string {
	mut conn := net.dial_tcp(addr)!
	defer {
		conn.close() or {}
	}
	conn.set_read_timeout(5 * time.second)
	conn.set_write_timeout(5 * time.second)
	conn.write_string(head)!
	mut raw := []u8{}
	for {
		mut tmp := []u8{len: 4096}
		n := conn.read(mut tmp) or { break }
		if n <= 0 {
			break
		}
		raw << tmp[..n]
	}
	text := raw.bytestr()
	return text.all_after('\r\n\r\n')
}

// test_remote_addr_is_the_peer_address checks that a handler sees the client's
// real ip:port, that remote_ip() drops the port, and that the historical
// `Remote-Addr` header still carries the bare ip.
fn test_remote_addr_is_the_peer_address() {
	mut server := remote_addr_server()
	t := spawn server.listen_and_serve()
	server.wait_till_running() or {
		assert false, 'server did not start: ${err}'
		return
	}
	defer {
		server.close()
		t.wait()
	}

	body := raw_request(server.addr, 'GET / HTTP/1.1\r\nHost: ${server.addr}\r\nConnection: close\r\n\r\n')!
	parts := body.split('|')
	assert parts.len == 3
	// remote_addr keeps the port, like Go's RemoteAddr.
	assert parts[0].starts_with('127.0.0.1:')
	assert parts[0].all_after('127.0.0.1:').int() > 0
	assert parts[1] == '127.0.0.1'
	assert parts[2] == '127.0.0.1'
}

// test_remote_addr_cannot_be_spoofed_by_a_client_header is the security case:
// header lookups return the first match, so a client that sends its own
// Remote-Addr must not be able to place it ahead of the server's. Both the
// canonical and a lowercase spelling are checked, since header names are
// case-insensitive on the wire.
fn test_remote_addr_cannot_be_spoofed_by_a_client_header() {
	mut server := remote_addr_server()
	t := spawn server.listen_and_serve()
	server.wait_till_running() or {
		assert false, 'server did not start: ${err}'
		return
	}
	defer {
		server.close()
		t.wait()
	}

	for spelling in ['Remote-Addr', 'remote-addr', 'REMOTE-ADDR'] {
		body := raw_request(server.addr, 'GET / HTTP/1.1\r\nHost: ${server.addr}\r\n${spelling}: 6.6.6.6\r\nConnection: close\r\n\r\n')!
		parts := body.split('|')
		assert parts.len == 3
		assert parts[0].starts_with('127.0.0.1:'), 'spoofed via ${spelling}: ${parts[0]}'
		assert parts[1] == '127.0.0.1', 'spoofed via ${spelling}: ${parts[1]}'
		assert parts[2] == '127.0.0.1', 'spoofed via ${spelling}: ${parts[2]}'
	}
}

// test_remote_addr_survives_a_full_header_table pins the fallback: the header
// array is a fixed 50 entries, so a request that fills it leaves no room for
// the mirrored `Remote-Addr`. The field must still be set (and adding it must
// not index past the end of the array).
fn test_remote_addr_survives_a_full_header_table() {
	mut server := remote_addr_server()
	t := spawn server.listen_and_serve()
	server.wait_till_running() or {
		assert false, 'server did not start: ${err}'
		return
	}
	defer {
		server.close()
		t.wait()
	}

	mut head := 'GET / HTTP/1.1\r\nHost: ${server.addr}\r\nConnection: close\r\n'
	for i in 0 .. max_headers - 2 {
		head += 'X-Filler-${i}: v\r\n'
	}
	head += '\r\n'
	body := raw_request(server.addr, head)!
	parts := body.split('|')
	assert parts.len == 3
	assert parts[0].starts_with('127.0.0.1:')
	assert parts[1] == '127.0.0.1'
}

// test_remote_addr_is_empty_for_client_requests documents that the field is
// server-side only: a request you build to send with the client has none.
fn test_remote_addr_is_empty_for_client_requests() {
	req := Request{
		url: 'http://example.com/'
	}
	assert req.remote_addr == ''
	assert req.remote_ip() == ''
}

fn test_remote_ip_strips_the_port_of_an_ipv6_address() {
	mut req := Request{}
	req.set_remote_addr('[::1]:40000')
	assert req.remote_addr == '[::1]:40000'
	assert req.remote_ip() == '::1'
	assert req.header.get_custom('Remote-Addr')? == '::1'
}

fn test_remote_ip_leaves_an_address_without_a_port_alone() {
	assert strip_addr_port('') == ''
	assert strip_addr_port('127.0.0.1') == '127.0.0.1'
	assert strip_addr_port('127.0.0.1:8080') == '127.0.0.1'
	assert strip_addr_port('::1') == '::1'
	assert strip_addr_port('[::1]:8080') == '::1'
}

// test_remote_ip_keeps_the_ipv6_zone covers scoped (link-local) peers. The RFC
// 4007 zone belongs to the address, not to the port, so dropping the port must
// not take it with it: two peers reached over different interfaces can share an
// address and are told apart only by the zone, which is also what makes the
// address dialable again.
fn test_remote_ip_keeps_the_ipv6_zone() {
	mut req := Request{}
	req.set_remote_addr('[fe80::1%3]:40000')
	assert req.remote_addr == '[fe80::1%3]:40000'
	assert req.remote_ip() == 'fe80::1%3'
	assert req.header.get_custom('Remote-Addr')? == 'fe80::1%3'

	assert strip_addr_port('[fe80::1%3]:8080') == 'fe80::1%3'
	assert strip_addr_port('fe80::1%3') == 'fe80::1%3'
}

// test_set_remote_addr_replaces_every_client_copy covers remove_custom_all's
// contract directly: every casing goes, and no empty placeholder is left for
// get_custom to return ahead of the real value.
fn test_set_remote_addr_replaces_every_client_copy() {
	mut req := Request{}
	req.header.add_custom('Remote-Addr', '6.6.6.6')!
	req.header.add_custom('X-Keep', 'me')!
	req.header.add_custom('remote-addr', '7.7.7.7')!
	req.set_remote_addr('10.0.0.5:1234')
	assert req.header.custom_values('Remote-Addr') == ['10.0.0.5']
	assert req.header.get_custom('Remote-Addr')? == '10.0.0.5'
	assert req.header.get_custom('X-Keep')? == 'me'
}

// test_reset_clears_remote_addr pins reset()'s contract for the new field: it
// runs the @[manualfree] cleanup and then restores defaults, so the address of
// the previous peer must not survive into the reused request.
fn test_reset_clears_remote_addr() {
	mut req := Request{}
	req.set_remote_addr('10.0.0.5:1234')
	assert req.remote_addr == '10.0.0.5:1234'
	assert req.header.get_custom('Remote-Addr')? == '10.0.0.5'

	req.reset()
	assert req.remote_addr == ''
	assert req.remote_ip() == ''
	assert req.header.get_custom('Remote-Addr') == none
}

// test_reset_with_remote_addr_sharing_the_header_buffer covers the aliasing case
// the cleanup has to survive: strip_addr_port returns its argument untouched
// when there is no port, so `remote_addr` and the mirrored `Remote-Addr` header
// value are then the *same* allocation. free() must release it once -- that is
// what the freed_ptrs map is for -- rather than twice. A heap string is required
// here; a literal would make string.free() a no-op and prove nothing.
fn test_reset_with_remote_addr_sharing_the_header_buffer() {
	heap_addr := '10.0.0' + '.5'
	mut req := Request{}
	req.set_remote_addr(heap_addr)
	assert req.remote_addr == '10.0.0.5'
	assert req.header.get_custom('Remote-Addr')? == '10.0.0.5'

	req.reset()
	assert req.remote_addr == ''
}
