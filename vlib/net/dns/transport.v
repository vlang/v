module dns

import net
import time

// NetTransport is the default Transport. It sends a query over UDP as is
// (RFC 1035 §4.2.1), and over TCP with a two-byte big-endian length prefix
// (§4.2.2) when a UDP response is truncated.
pub struct NetTransport {}

// exchange_udp sends `query` to `server`, then waits up to `timeout` for the
// first datagram that answers it: one that comes from the address and port the
// query was sent to, and carries the same transaction id with the QR bit set.
// Other datagrams are skipped.
pub fn (mut t NetTransport) exchange_udp(server string, query []u8, timeout time.Duration) ![]u8 {
	if query.len < 2 {
		return error('net.dns: query too short')
	}
	mut conn := net.dial_udp(server)!
	defer {
		conn.close() or {}
	}
	remote := conn.sock.remote() or { return error('net.dns: cannot resolve ${server}') }
	expected := remote.str()
	conn.set_write_timeout(timeout)
	conn.write(query)!
	deadline := time.now().add(timeout)
	mut buf := []u8{len: max_udp_payload}
	for {
		conn.set_read_timeout(remaining_until(deadline) or {
			return error('net.dns: no response from ${server} within ${timeout}')
		})
		n, from := conn.read(mut buf)!
		if from.str() != expected {
			continue
		}
		if n >= header_size && buf[0] == query[0] && buf[1] == query[1] && buf[2] & 0x80 != 0 {
			return buf[..n].clone()
		}
	}
	return error('net.dns: no response from ${server}')
}

// exchange_tcp sends `query` to `server` over TCP, with the two-byte length
// prefix of RFC 1035 §4.2.2, and returns the response, whose reads `timeout`
// bounds together.
pub fn (mut t NetTransport) exchange_tcp(server string, query []u8, timeout time.Duration) ![]u8 {
	if query.len > 0xffff {
		return error('net.dns: query too long for TCP framing')
	}
	deadline := time.now().add(timeout)
	mut conn := net.dial_tcp(server)!
	defer {
		conn.close() or {}
	}
	mut framed := []u8{cap: 2 + query.len}
	framed << u8(query.len >> 8)
	framed << u8(query.len & 0xFF)
	framed << query
	conn.set_write_timeout(remaining_until(deadline)!)
	conn.write(framed)!

	mut len_buf := [u8(0), 0]
	read_full(mut conn, mut len_buf, deadline)!
	body_len := int((u32(len_buf[0]) << 8) | u32(len_buf[1]))
	if body_len < header_size {
		return error('net.dns: TCP response of ${body_len} bytes is shorter than a header')
	}
	mut body := []u8{len: body_len}
	read_full(mut conn, mut body, deadline)!
	return body
}

// remaining_until returns the time left before `deadline`, or an error once it
// has passed.
fn remaining_until(deadline time.Time) !time.Duration {
	left := deadline - time.now()
	if left <= 0 {
		return error('net.dns: timed out')
	}
	return left
}

// read_full keeps reading from the TCP stream until `dst` is filled, since a
// single `read` may return fewer bytes than requested, and fails at `deadline`.
fn read_full(mut conn net.TcpConn, mut dst []u8, deadline time.Time) ! {
	mut got := 0
	for got < dst.len {
		conn.set_read_timeout(remaining_until(deadline)!)
		n := conn.read(mut dst[got..])!
		if n <= 0 {
			return error('net.dns: connection closed before ${dst.len} bytes')
		}
		got += n
	}
}
