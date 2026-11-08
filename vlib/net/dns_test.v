module net

import time

fn mx_response(id u16) []u8 {
	mut b := []u8{}
	b << [u8(id >> 8), u8(id), 0x81, 0x80, 0, 1, 0, 1, 0, 0, 0, 0]
	// question: example.com MX IN
	b << [u8(7), `e`, `x`, `a`, `m`, `p`, `l`, `e`, 3, `c`, `o`, `m`, 0]
	b << [u8(0), 15, 0, 1]
	// answer: pointer to qname, MX IN, ttl 300, pref 10, mail.example.com
	b << [u8(0xc0), 0x0c, 0, 15, 0, 1, 0, 0, 1, 0x2c, 0, 20, 0, 10]
	b << [u8(4), `m`, `a`, `i`, `l`, 7, `e`, `x`, `a`, `m`, `p`, `l`, `e`, 3, `c`, `o`, `m`, 0]
	return b
}

fn test_build_query_shape() {
	q := build_dns_query(0x1234, 'example.com', dns_type_mx)!
	assert q[0] == 0x12 && q[1] == 0x34
	// flags: recursion desired
	assert q[2] == 0x01 && q[3] == 0x00
	// qdcount 1
	assert q[4] == 0 && q[5] == 1
	// qname
	assert q[12] == 7
	// qtype MX, qclass IN at the end
	assert q[q.len - 4] == 0 && q[q.len - 3] == 15
	assert q[q.len - 2] == 0 && q[q.len - 1] == 1
}

fn test_encode_dns_name() {
	assert encode_dns_name('example.com')! == [u8(7), `e`, `x`, `a`, `m`, `p`, `l`, `e`, 3, `c`,
		`o`, `m`, 0]
	assert encode_dns_name('example.com.')! == encode_dns_name('example.com')!
	assert encode_dns_name('')! == [u8(0)]
	if _ := encode_dns_name('a..b') {
		assert false, 'expected label error'
	} else {
		assert err.msg().contains('invalid DNS label')
	}
}

fn test_parse_mx_response() {
	resp := parse_dns_response(mx_response(0x1234), 0x1234)!
	assert resp.id == 0x1234
	assert resp.answers.len == 1
	a := resp.answers[0]
	assert a.name == 'example.com'
	assert a.typ == dns_type_mx
	assert a.ttl == 300
	match a.data {
		DnsMx {
			assert a.data.pref == 10
			assert a.data.host == 'mail.example.com'
		}
		else {
			assert false, 'expected MX'
		}
	}
}

fn test_parse_rejects_id_mismatch() {
	if _ := parse_dns_response(mx_response(0x1234), 0x9999) {
		assert false, 'expected id error'
	} else {
		assert err.msg().contains('id mismatch')
	}
}

fn test_parse_rejects_rcode() {
	mut pkt := mx_response(0x1234)
	pkt[3] = 3 // NXDOMAIN
	if _ := parse_dns_response(pkt, 0x1234) {
		assert false, 'expected rcode error'
	} else {
		assert err.msg().contains('rcode 3')
	}
}

fn test_parse_rejects_truncated() {
	if _ := parse_dns_response([u8(1), 2, 3], 0x0102) {
		assert false, 'expected truncation error'
	} else {
		assert err.msg().contains('truncated')
	}
}

fn test_parse_a_and_txt_and_srv() {
	// A record for example.com -> 93.184.216.34, name via pointer.
	mut b := []u8{}
	b << [u8(0x22), 0x22, 0x81, 0x80, 0, 1, 0, 1, 0, 0, 0, 0]
	b << [u8(7), `e`, `x`, `a`, `m`, `p`, `l`, `e`, 3, `c`, `o`, `m`, 0]
	b << [u8(0), 1, 0, 1]
	b << [u8(0xc0), 0x0c, 0, 1, 0, 1, 0, 0, 0, 60, 0, 4, 93, 184, 216, 34]
	resp := parse_dns_response(b, 0x2222)!
	assert resp.answers.len == 1
	match resp.answers[0].data {
		DnsA {
			assert resp.answers[0].data.ip == '93.184.216.34'
		}
		else {
			assert false, 'expected A'
		}
	}
	// TXT record with two character-strings.
	mut t := []u8{}
	t << [u8(0x33), 0x33, 0x81, 0x80, 0, 1, 0, 1, 0, 0, 0, 0]
	t << [u8(7), `e`, `x`, `a`, `m`, `p`, `l`, `e`, 3, `c`, `o`, `m`, 0]
	t << [u8(0), 16, 0, 1]
	t << [u8(0xc0), 0x0c, 0, 16, 0, 1, 0, 0, 0, 60, 0, 12]
	t << [u8(5), `h`, `e`, `l`, `l`, `o`, 5, `w`, `o`, `r`, `l`, `d`]
	tresp := parse_dns_response(t, 0x3333)!
	match tresp.answers[0].data {
		DnsTxt {
			assert tresp.answers[0].data.strings == ['hello', 'world']
		}
		else {
			assert false, 'expected TXT'
		}
	}
	// SRV record.
	mut s := []u8{}
	s << [u8(0x44), 0x44, 0x81, 0x80, 0, 1, 0, 1, 0, 0, 0, 0]
	s << [u8(7), `e`, `x`, `a`, `m`, `p`, `l`, `e`, 3, `c`, `o`, `m`, 0]
	s << [u8(0), 33, 0, 1]
	s << [u8(0xc0), 0x0c, 0, 33, 0, 1, 0, 0, 0, 60, 0, 23]
	s << [u8(0), 0, 0, 5, 0x14, 0x66]
	s << [u8(3), `s`, `r`, `v`, 7, `e`, `x`, `a`, `m`, `p`, `l`, `e`, 3, `c`, `o`, `m`, 0]
	sresp := parse_dns_response(s, 0x4444)!
	match sresp.answers[0].data {
		DnsSrv {
			assert sresp.answers[0].data.priority == 0
			assert sresp.answers[0].data.weight == 5
			assert sresp.answers[0].data.port == 0x1466
			assert sresp.answers[0].data.target == 'srv.example.com'
		}
		else {
			assert false, 'expected SRV'
		}
	}
}

fn test_decode_name_pointer_loop() {
	// Pointer to itself.
	pkt := [u8(0xc0), 0x00, 0, 1, 0, 1]
	if _, _ := decode_dns_name(pkt, 0) {
		assert false, 'expected loop error'
	} else {
		assert err.msg().contains('loop') || err.msg().contains('truncated')
	}
}

fn serve_one_dns(mut srv UdpConn, resp_id_from_query bool) {
	mut buf := []u8{len: 512}
	n, client := srv.read(mut buf) or { return }
	mut resp := mx_response(0)
	if resp_id_from_query && n >= 2 {
		resp[0] = buf[0]
		resp[1] = buf[1]
	}
	srv.write_to(client, resp) or {}
	srv.close() or {}
}

fn test_lookup_mx_roundtrip() {
	mut srv := listen_udp('127.0.0.1:0')!
	port := addr_from_socket_handle(srv.sock.handle).port()!
	t := spawn serve_one_dns(mut srv, true)
	time.sleep(50 * time.millisecond)
	c := new_dns_client('127.0.0.1:${port}')
	mxs := c.lookup_mx('example.com')!
	assert mxs.len == 1
	assert mxs[0].pref == 10
	assert mxs[0].host == 'mail.example.com'
	t.wait()
}
