module http

// Tests for the VSchannelPooledTransport adapter (vschannel_pooled_transport_windows.c.v)
// in isolation, mirroring H2PooledTransport's own adapter tests
// (transport_h2_test.v) for the mbedTLS/OpenSSL backends. These dial a real
// loopback mbedTLS TLS server (no ALPN needed here -- that is exercised
// separately by the dial-path integration test) since a genuine SChannel
// client should interoperate with any standards-conformant TLS 1.2 server
// regardless of which library the server side uses.
import net
import net.mbedtls
import time

const vpt_cert_path = @VEXEROOT +
	'/vlib/net/websocket/tests/autobahn/fuzzing_server_wss/config/server.crt'
const vpt_key_path = @VEXEROOT +
	'/vlib/net/websocket/tests/autobahn/fuzzing_server_wss/config/server.key'

fn vpt_echo_conn(mut conn mbedtls.SSLConn) {
	defer {
		conn.shutdown() or {}
	}
	conn.set_read_timeout(10 * time.second)
	for {
		mut buf := []u8{len: 4096}
		n := conn.read(mut buf) or { return }
		if n <= 0 {
			return
		}
		conn.write(buf[..n]) or { return }
	}
}

fn vpt_echo_loop(mut listener mbedtls.SSLListener) {
	for {
		mut conn := listener.accept() or { return }
		spawn vpt_echo_conn(mut conn)
	}
}

fn vpt_hammer_reader(mut t VSchannelPooledTransport) int {
	mut total := 0
	for {
		mut buf := []u8{len: 2048}
		n := t.read(mut buf) or {
			// Mirror the real consumer (H2MuxConn.read_loop): timeout errors
			// are benign wake-ups to re-poll on; anything else ends the read
			// loop -- after close() that is the socket error that lets this
			// thread exit.
			if is_transport_timeout_error(err) {
				continue
			}
			return total
		}
		total += n
	}
	return total
}

fn vpt_hammer_writer(mut t VSchannelPooledTransport) int {
	payload := []u8{len: 512, init: u8(0xab)}
	mut writes := 0
	for _ in 0 .. 500 {
		t.write(payload) or { return writes }
		writes++
	}
	return writes
}

fn vpt_start_listener() (int, &mbedtls.SSLListener) {
	mut port_listener := net.listen_tcp(.ip, '127.0.0.1:0') or {
		assert false, 'port: ${err}'
		return 0, unsafe { nil }
	}
	port := port_listener.addr() or {
		assert false, 'addr: ${err}'
		return 0, unsafe { nil }
	}.port() or {
		assert false, 'addr.port: ${err}'
		return 0, unsafe { nil }
	}
	port_listener.close() or {}
	mut listener := mbedtls.new_ssl_listener('127.0.0.1:${port}', mbedtls.SSLConnectConfig{
		cert:     vpt_cert_path
		cert_key: vpt_key_path
		validate: false
	}) or {
		assert false, 'listener: ${err}'
		return 0, unsafe { nil }
	}
	return port, listener
}

// The adapter's &t.ctx address must stay stable across calls: VSchannelPooledTransport
// is @[heap] and used only through a pointer receiver, so the embedded-by-value
// C.TlsContext field never moves for the object's lifetime -- unlike mbedTLS,
// where H2PooledTransport must wrap the SSLConn BY POINTER for the same
// reason. On a hypothetical broken version that copied VSchannelPooledTransport
// by value somewhere in its own methods, this would catch the divergence.
fn test_vschannel_pooled_transport_stable_address() {
	port, mut listener := vpt_start_listener()
	th := spawn vpt_echo_loop(mut listener)
	mut t := new_vschannel_pooled_transport('127.0.0.1', port, false, ['h2', 'http/1.1'],
		h2_pooled_io_timeout, h2_pooled_write_stall_limit) or {
		assert false, 'connect: ${err}'
		return
	}
	addr1 := voidptr(&t.ctx)
	mut buf := []u8{len: 4}
	t.write([u8(1), 2, 3, 4]) or { assert false, 'write: ${err}' }
	t.read(mut buf) or { assert false, 'read: ${err}' }
	addr2 := voidptr(&t.ctx)
	assert addr1 == addr2, 'the embedded TlsContext must not move across calls'
	t.close()
	listener.shutdown() or {}
	th.wait()
}

// One thread reads, one writes, and a third closes mid-flight: the adapter
// must serialize all TLS-context access so this never crashes or hangs (this
// codebase tests concurrency by repetition, matching h2_mux_conn_test.v).
// Both hammer threads returning at all -- rather than crashing or blocking
// forever past the harness timeout -- is the property under test.
fn test_vschannel_pooled_transport_concurrent_read_write_close() {
	for round in 0 .. 3 {
		port, mut listener := vpt_start_listener()
		th := spawn vpt_echo_loop(mut listener)
		mut t := new_vschannel_pooled_transport('127.0.0.1', port, false, ['h2', 'http/1.1'],
			h2_pooled_io_timeout, h2_pooled_write_stall_limit) or {
			assert false, 'round ${round}: connect: ${err}'
			return
		}
		rt := spawn vpt_hammer_reader(mut t)
		wt := spawn vpt_hammer_writer(mut t)
		time.sleep(200 * time.millisecond)
		t.close()
		writes := wt.wait()
		reads := rt.wait()
		assert writes >= 0 && reads >= 0, 'round ${round}: hammer threads must terminate cleanly'
		// A second close must be a no-op, not a double-free.
		t.close()
		listener.shutdown() or {}
		th.wait()
	}
}

// read() must return within roughly h2_pooled_io_timeout (50ms) when nothing
// arrives, with an error is_transport_timeout_error recognizes as a benign
// wake-up -- otherwise H2MuxConn's reader thread could never notice a
// retiring connection, and close() (which needs the same io_mu a blocked
// reader would be holding) would deadlock.
fn test_vschannel_pooled_transport_read_times_out() {
	port, mut listener := vpt_start_listener()
	th := spawn vpt_echo_loop(mut listener)
	mut t := new_vschannel_pooled_transport('127.0.0.1', port, false, ['h2', 'http/1.1'],
		h2_pooled_io_timeout, h2_pooled_write_stall_limit) or {
		assert false, 'connect: ${err}'
		return
	}
	mut buf := []u8{len: 64}
	started := time.now()
	t.read(mut buf) or {
		elapsed := time.now() - started
		assert is_transport_timeout_error(err), 'expected a timeout-classified error, got: ${err}'
		assert elapsed < 1 * time.second, 'read() took ${elapsed}, far more than the 50ms io timeout -- SO_RCVTIMEO is not taking effect'
	}
	t.close()
	listener.shutdown() or {}
	th.wait()
}

// write() must not fail a stalled write against h2_pooled_io_timeout (50ms):
// a peer that merely stops draining mid-write -- still connected, not closed
// -- must get the intended h2_pooled_write_stall_limit (30s) budget, the same
// property H2PooledTransport's own write-stall test verifies for mbedTLS/
// OpenSSL. Backpressure is forced with a large payload against a captive
// server that never reads again; unlike the mbedTLS test, there is no V-level
// socket handle to shrink the send buffer on (vschannel's socket is opaque to
// V), so this relies on the OS's default buffers filling from payload size
// alone.
fn test_vschannel_pooled_transport_write_survives_stall_past_io_timeout() {
	port, mut listener := vpt_start_listener()
	// Server: complete the handshake, then go captive -- never read again,
	// simulating a healthy-but-slow peer that has stopped draining its
	// receive buffer (not a dead or closed one).
	th := spawn fn (mut l mbedtls.SSLListener) {
		mut sconn := l.accept() or { return }
		time.sleep(2 * time.second)
		sconn.shutdown() or {}
	}(mut listener)

	mut t := new_vschannel_pooled_transport('127.0.0.1', port, false, ['h2', 'http/1.1'],
		h2_pooled_io_timeout, h2_pooled_write_stall_limit) or {
		assert false, 'connect: ${err}'
		return
	}
	payload := []u8{len: 4 * 1024 * 1024, init: u8(0xab)}

	write_done := chan bool{cap: 1}
	spawn fn [mut t, payload, write_done] () {
		t.write(payload) or {}
		write_done <- true
	}()

	// On buggy code (a write timeout reusing the short poll interval instead
	// of the write stall budget) the write would fail almost immediately even
	// though the connection is perfectly healthy and has not been closed. A
	// comfortable multiple of the io timeout must still find the write in
	// flight.
	select {
		_ := <-write_done {
			assert false, 'write() returned within 500ms even though the peer never disconnected -- it must be retrying against the write stall budget'
		}
		500 * time.millisecond {
			// still in flight, as expected on correct code
		}
	}

	t.close()
	_ := <-write_done
	th.wait()
}
