// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

import net
import net.quic
import time

// h3_default_own_qpack_max_table_capacity is this client's own QPACK
// decoder's dynamic-table capacity ceiling (RFC 9204 §3.2.3), and what is
// advertised to the peer via SETTINGS_QPACK_MAX_TABLE_CAPACITY. A modest,
// widely-used default, well under QPACK's own 64 MiB hard cap
// (quic.qpack_max_dynamic_table_capacity_bytes) -- generous enough for
// ordinary header reuse across one connection's requests, small enough not
// to commit meaningful memory per idle pooled connection.
const h3_default_own_qpack_max_table_capacity = u64(4096)

// h3_default_own_qpack_blocked_streams is this client's own advertised
// SETTINGS_QPACK_BLOCKED_STREAMS (RFC 9204 §5): how many streams this
// connection's OWN QpackDecoder may have blocked at once, waiting on a
// Required Insert Count the peer's encoder-stream instructions have not
// caught up to yet. h3_control_stream.v's apply_peer_settings notes this
// connection's QpackEncoder never risks blocking a stream when THIS
// endpoint is the encoder; this constant is the mirror-image bound for
// when the PEER is encoding to us. blocked_sections (h3_conn.v) has no
// independent cap of its own, so this is the only real limit on it -- a
// generous, commonly used default.
const h3_default_own_qpack_blocked_streams = u64(100)

// h3_datagram_buf_size bounds one UDP read: generous enough for any legal
// UDP payload a QUIC peer could send (real datagrams are normally far
// smaller, bounded by path MTU), so a read is never truncated.
const h3_datagram_buf_size = 65535

// h3_now_ms returns the current time as milliseconds on a monotonic
// clock -- the unit and clock domain every quic.QuicConn/quic.H3Conn `now
// u64` parameter expects (idle-timeout and loss-detection arithmetic is
// relative, not wall-clock; see quic.dial's own connection_start field).
// Deliberately never wall-clock based: a wall-clock step (NTP correction,
// DST, manual clock change) must never corrupt that arithmetic.
fn h3_now_ms() u64 {
	return time.sys_mono_now() / 1_000_000
}

// h3_default_own_settings builds this client's own outgoing SETTINGS
// frame contents (RFC 9114 §7.2.4): just the two QPACK parameters RFC 9204
// §5 defines, at this file's own chosen defaults above. An H3Conn does not
// invent these values itself (h3_conn.v's own H3ConnParams doc comment) --
// this is where a real client finally commits to concrete numbers.
fn h3_default_own_settings() []quic.H3Setting {
	return [
		quic.H3Setting{
			identifier: quic.qpack_settings_max_table_capacity_id
			value:      h3_default_own_qpack_max_table_capacity
		},
		quic.H3Setting{
			identifier: quic.qpack_settings_blocked_streams_id
			value:      h3_default_own_qpack_blocked_streams
		},
	]
}

// H3UdpConnTransport adapts a connect()-ed net.UdpConn into H3UdpTransport
// (h3_mux_conn.v) -- the one production implementation of that interface.
// Unlike H2PooledTransport's adapter (h2_pooled_transport.v), this needs no
// locking of its own: nothing else ever touches this UdpConn concurrently
// with H3MuxConn's single driver thread (h3_mux_conn.v's own module doc
// comment), so there is no reader/writer race to guard against here.
@[heap]
struct H3UdpConnTransport {
mut:
	conn &net.UdpConn
}

fn (mut t H3UdpConnTransport) read(mut buf []u8) !int {
	n, _ := t.conn.read(mut buf)!
	return n
}

fn (mut t H3UdpConnTransport) write(buf []u8) !int {
	return t.conn.write(buf)
}

fn (mut t H3UdpConnTransport) set_read_timeout(timeout time.Duration) {
	t.conn.set_read_timeout(timeout)
}

fn (mut t H3UdpConnTransport) close() ! {
	return t.conn.close()
}

// h3_dial_udp_and_open dials a UDP socket to host:port -- connected, so
// later write()s need no explicit destination address -- starts a QUIC
// client handshake over it, and wraps the result in an H3Conn using this
// file's own fixed QPACK defaults above. The caller owns the returned
// H3UdpTransport/QuicConn/H3Conn from this point on (closing the transport
// is what H3MuxConn.fail_conn uses to tear everything down). The transport
// is returned as the H3UdpTransport interface (h3_mux_conn.v), wrapped via
// H3UdpConnTransport, rather than a bare &net.UdpConn -- that is the seam
// h3_mux_conn_test.v substitutes an in-memory fake through, matching this
// codebase's own H2Transport/H2PooledTransport precedent (h2_conn.v/
// h2_pooled_transport.v).
//
// Nothing is read from the socket here: quic.dial's own first flight (a
// padded Initial packet) is written synchronously on the CALLING thread,
// matching new_h2_mux_conn's own "spawn only after setup" shape -- the
// driver thread H3MuxConn's constructor starts (h3_mux_conn.v) owns every
// read from this point on, never this function.
fn h3_dial_udp_and_open(server_name string, host string, port int, ca_bundle_pem string, alpn_protocols []string, tp quic.QuicTransportParameters) !(H3UdpTransport, &quic.QuicConn, &quic.H3Conn) {
	mut udp := net.dial_udp('${host}:${port}')!
	mut transport := H3UdpTransport(&H3UdpConnTransport{
		conn: udp
	})
	now := h3_now_ms()
	mut qc, first_datagram := quic.dial(quic.DialParams{
		server_name:          server_name
		ca_bundle_pem:        ca_bundle_pem
		alpn_protocols:       alpn_protocols
		transport_parameters: tp
	}, now) or {
		transport.close() or {}
		return err
	}
	transport.write(first_datagram.bytes) or {
		transport.close() or {}
		return err
	}
	mut h3 := quic.new_h3_conn(mut qc, quic.H3ConnParams{
		settings:                     h3_default_own_settings()
		own_qpack_max_table_capacity: h3_default_own_qpack_max_table_capacity
	})
	return transport, qc, h3
}
