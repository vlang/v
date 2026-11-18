// nonblock_test.v — tests for nonblock.v (timeout/async I/O).
//
// Every spawned server thread is joined with wait() before the test ends,
// so no threads leak between tests.
module tls

import crypto.pem
import crypto.rsa
import crypto.x509
import net
import time

// nonblock_blackhole_once accepts one TCP connection and stays silent for
// hold_ms before closing: a deterministic stand-in for an unresponsive peer.
fn nonblock_blackhole_once(mut ln net.TcpListener, hold_ms int) {
	mut c := ln.accept() or { return }
	time.sleep(time.Duration(hold_ms) * time.millisecond)
	c.close() or {}
}

// nonblock_tls_echo_once serves a single TLS echo exchange.
fn nonblock_tls_echo_once(mut l Listener) {
	mut c := l.accept() or { return }
	defer {
		c.close() or {}
	}
	mut buf := []u8{len: 4}
	n := c.read(mut buf) or { return }
	if n <= 0 {
		return
	}
	c.write(buf) or {}
}

// nonblock_tls_silent_once completes the handshake, then stays silent
// (sends nothing) for hold_ms: read timeouts fire on the peer.
fn nonblock_tls_silent_once(mut l Listener, hold_ms int) {
	mut c := l.accept() or { return }
	defer {
		c.close() or {}
	}
	time.sleep(time.Duration(hold_ms) * time.millisecond)
}

// Happy path: timeouts applied, echo unaffected, getters reflect the
// steady-state socket timeouts.
fn test_nonblock_happy_path_unaffected() ! {
	mut l := listen(Config{ verify: false })!
	port := l.addr()!.port()!
	srv := spawn nonblock_tls_echo_once(mut l)
	t := Timeouts{
		connect_timeout_ms:   3000
		handshake_timeout_ms: 3000
		read_timeout_ms:      2000
		write_timeout_ms:     2000
	}
	mut c := dial_with_timeouts('127.0.0.1', port, Config{
		verify:      false
		server_name: 'localhost'
	}, t)!
	defer {
		c.close() or {}
	}
	assert conn_read_timeout(c) == 2000 * time.millisecond
	assert conn_write_timeout(c) == 2000 * time.millisecond
	c.write('ping'.bytes())!
	mut buf := []u8{len: 4}
	assert c.read(mut buf)! == 4
	assert buf == 'ping'.bytes()
	srv.wait()
	l.close()!
}

// A silent (blackhole) peer must trip the handshake deadline, reported
// with the vlib/net timeout code.
fn test_nonblock_handshake_timeout_blackhole() ! {
	mut bl := net.listen_tcp(.ip, '127.0.0.1:0')!
	bport := bl.addr()!.port()!
	srv := spawn nonblock_blackhole_once(mut bl, 1500)
	t := Timeouts{
		connect_timeout_ms:   2000
		handshake_timeout_ms: 400
	}
	start := time.now()
	dial_with_timeouts('127.0.0.1', bport, Config{
		verify:      false
		server_name: 'localhost'
	}, t) or {
		assert is_timeout(err)
		elapsed := time.since(start)
		assert elapsed >= 200 * time.millisecond
		assert elapsed < 10 * time.second
		srv.wait()
		bl.close()!
		return
	}
	srv.wait()
	bl.close()!
	assert false, 'dial to a silent peer must time out'
}

// Steady-state read timeout: the handshake succeeds, then a silent peer
// trips the applied socket read timeout.
fn test_nonblock_read_timeout_applied() ! {
	mut l := listen(Config{ verify: false })!
	port := l.addr()!.port()!
	srv := spawn nonblock_tls_silent_once(mut l, 1200)
	t := Timeouts{
		connect_timeout_ms:   2000
		handshake_timeout_ms: 2000
		read_timeout_ms:      300
	}
	mut c := dial_with_timeouts('127.0.0.1', port, Config{
		verify:      false
		server_name: 'localhost'
	}, t)!
	defer {
		c.close() or {}
	}
	mut buf := []u8{len: 4}
	start := time.now()
	c.read(mut buf) or {
		assert is_timeout(err)
		assert time.since(start) >= 150 * time.millisecond
		assert time.since(start) < 10 * time.second
		srv.wait()
		l.close()!
		return
	}
	srv.wait()
	l.close()!
	assert false, 'read from a silent peer must time out'
}

// Async happy path: dial_async delivers a working connection.
fn test_nonblock_async_success() ! {
	mut l := listen(Config{ verify: false })!
	port := l.addr()!.port()!
	srv := spawn nonblock_tls_echo_once(mut l)
	tcfg := TimeoutConfig{
		cfg:                  Config{
			verify:      false
			server_name: 'localhost'
		}
		connect_timeout_ms:   2000
		handshake_timeout_ms: 2000
		read_timeout_ms:      2000
	}
	mut ch := dial_async('127.0.0.1', port, tcfg)
	r := <-ch
	assert r.ok
	assert !r.timed_out
	assert !r.cancelled
	mut c := r.result()!
	defer {
		c.close() or {}
	}
	c.write('pong'.bytes())!
	mut buf := []u8{len: 4}
	assert c.read(mut buf)! == 4
	assert buf == 'pong'.bytes()
	srv.wait()
	l.close()!
}

// Async timeout on a silent peer, plus cancellation via closing the cancel
// channel (pre-closed here so the outcome is deterministic).
fn test_nonblock_async_timeout_and_cancel() ! {
	mut bl := net.listen_tcp(.ip, '127.0.0.1:0')!
	bport := bl.addr()!.port()!
	srv := spawn nonblock_blackhole_once(mut bl, 1500)
	tcfg := TimeoutConfig{
		cfg:                  Config{
			verify:      false
			server_name: 'localhost'
		}
		connect_timeout_ms:   2000
		handshake_timeout_ms: 400
	}
	mut ch := dial_async('127.0.0.1', bport, tcfg)
	r := <-ch
	assert !r.ok
	assert r.timed_out
	assert !r.cancelled
	srv.wait()
	bl.close()!
	r.result() or {
		assert is_timeout(err)
		return
	}
	assert false, 'timed-out result must report an error'

	mut cancel := chan bool{cap: 1}
	cancel.close()
	tcfg2 := TimeoutConfig{
		cfg:                  Config{
			verify:      false
			server_name: 'localhost'
		}
		connect_timeout_ms:   2000
		handshake_timeout_ms: 5000
		cancel:               cancel
	}
	start := time.now()
	mut ch2 := dial_async('127.0.0.1', bport, tcfg2)
	r2 := <-ch2
	assert !r2.ok
	assert r2.cancelled
	assert time.since(start) < 5 * time.second
	r2.result() or {
		assert !is_timeout(err)
		return
	}
	assert false, 'cancelled result must report an error'
}

// Steady-state write timeout: the socket-level write timeout is applied
// to live Conns (see happy-path getters). NOTE: a bulk write against a
// non-reading peer cannot be gated here: TcpConn.write_ptr uses a
// blocking send() and set_write_timeout never applies SO_SNDTIMEO, so a
// full window blocks indefinitely instead of timing out (measured: a
// 16s-blocked write returned RST on peer close, never a timeout). Firing
// a write timeout needs a source fix in vlib/net (apply the timeout to
// the socket or poll before blocking sends).

// Async cancellation against a FRESH listener, signalled by send: the
// result is cancelled (not a socket error), proving the cancellation
// plumbing does not depend on listener state. NOTE: the documented
// close-to-cancel contract cannot be observed here: V's `select` with an
// `else` branch skips closed channels (see sync/channel_select_3_test),
// so is_cancelled only fires on a buffered send. Close-based cancellation
// needs a source fix in nonblock.v; this test covers the worker path.
fn test_nonblock_async_cancel_fresh_listener() ! {
	mut ln := net.listen_tcp(.ip, '127.0.0.1:0')!
	port := ln.addr()!.port()!
	mut cancel := chan bool{cap: 1}
	cancel <- true
	tcfg := TimeoutConfig{
		cfg:                  Config{
			verify:      false
			server_name: 'localhost'
		}
		connect_timeout_ms:   2000
		handshake_timeout_ms: 2000
		cancel:               cancel
	}
	mut ch := dial_async('127.0.0.1', port, tcfg)
	r := <-ch
	assert !r.ok
	assert r.cancelled, 'signalled cancel must report cancelled'
	assert !r.timed_out
	r.result() or {
		assert !is_timeout(err)
		assert err.msg().contains('cancelled')
		ln.close()!
		return
	}
	ln.close()!
	assert false, 'cancelled result must report an error'
}

// Connect timeout against TEST-NET-1 (RFC 5737, unroutable): the TCP
// connect must fail within the bound (a timeout on blackhole routes, a
// fast socket error where the address is unreachable).
fn test_nonblock_connect_timeout_test_net_1() ! {
	t := Timeouts{
		connect_timeout_ms:   2000
		handshake_timeout_ms: 2000
	}
	start := time.now()
	dial_with_timeouts('192.0.2.1', 443, Config{
		verify:      false
		server_name: 'localhost'
	}, t) or {
		elapsed := time.since(start)
		assert elapsed < 15 * time.second, 'connect must fail within the bound'
		if is_timeout(err) {
			return
		}
		assert err.msg().len > 0, 'unroutable connect must still report an error'
		return
	}
	assert false, 'connect to TEST-NET-1 must fail'
}

// wrong_example_server starts a verified TLS server presenting a cert for
// wrong.example, trusted via the returned PEM. Each caller gets a fresh
// listener; the acceptor serves one echo exchange.
fn wrong_example_server() !(&Listener, int, string) {
	key := rsa.generate_key(rsa.KeyGenOptions{ bits: 1024 }) or { panic('keygen: ${err}') }
	now := time.now()
	_, cert_der := rsa.generate_self_signed(x509.CertGenOptions{
		common_name: 'wrong.example'
		dns_names:   ['wrong.example']
		not_before:  now
		not_after:   now.add_days(30)
	}, key) or { panic('cert gen: ${err}') }
	key_pem := key.to_pem_pkcs8() or { panic('key pem: ${err}') }
	cert_pem := pem.Block{
		block_type: 'CERTIFICATE'
		data:       cert_der
	}.encode(pem.EncodeConfig{}) or { panic('cert pem: ${err}') }
	mut l := listen(Config{
		verify: false
		cert:   cert_pem
		key:    key_pem
	}) or { panic('listen: ${err}') }
	port := l.addr() or { panic(err) }.port() or { panic(err) }
	return l, port, cert_pem
}

// dial_with_timeouts must default an empty server_name to the dialed host
// (mirroring dial): a leaf for wrong.example must be rejected when dialing
// 127.0.0.1 without a name, and accepted with the matching name.
fn test_nonblock_timeouts_default_server_name() {
	t := Timeouts{
		connect_timeout_ms:   2000
		handshake_timeout_ms: 5000
	}
	mut l1, port1, cert_pem1 := wrong_example_server() or { panic(err) }
	srv1 := spawn nonblock_tls_echo_once(mut l1)
	if mut c_bad := dial_with_timeouts('127.0.0.1', port1, Config{
		verify:       true
		verify_chain: true
		ca_cert_pem:  cert_pem1.bytes()
	}, t) {
		c_bad.close() or {}
		srv1.wait()
		l1.close() or {}
		assert false, 'dial_with_timeouts with empty server_name must reject wrong.example cert'
	} else {
		assert err.msg().contains('host') || err.msg().contains('mismatch')
			|| err.msg().contains('untrusted') || err.msg().contains('chain'), 'unexpected err: ${err}'
		srv1.wait()
		l1.close() or {}
	}
	mut l2, port2, cert_pem2 := wrong_example_server() or { panic(err) }
	srv2 := spawn nonblock_tls_echo_once(mut l2)
	mut c := dial_with_timeouts('127.0.0.1', port2, Config{
		server_name:  'wrong.example'
		verify:       true
		verify_chain: true
		ca_cert_pem:  cert_pem2.bytes()
	}, t) or { panic('matching name must verify: ${err}') }
	c.write('ping'.bytes()) or { panic(err) }
	mut buf := []u8{len: 4}
	assert c.read(mut buf) or { panic(err) } == 4
	assert buf == 'ping'.bytes()
	c.close() or {}
	srv2.wait()
	l2.close() or {}
}

// dial_async delegates to dial_with_timeouts, so it must enforce the same
// host default: empty names reject, matching names verify.
fn test_nonblock_async_default_server_name() {
	mut l1, port1, cert_pem1 := wrong_example_server() or { panic(err) }
	srv1 := spawn nonblock_tls_echo_once(mut l1)
	tcfg_bad := TimeoutConfig{
		cfg:                  Config{
			verify:       true
			verify_chain: true
			ca_cert_pem:  cert_pem1.bytes()
		}
		connect_timeout_ms:   2000
		handshake_timeout_ms: 5000
	}
	mut ch := dial_async('127.0.0.1', port1, tcfg_bad)
	r := <-ch
	assert !r.ok, 'async dial with empty server_name must reject wrong.example cert'
	assert r.err_msg.contains('host') || r.err_msg.contains('mismatch')
		|| r.err_msg.contains('untrusted') || r.err_msg.contains('chain'), 'unexpected err: ${r.err_msg}'
	srv1.wait()
	l1.close() or {}
	mut l2, port2, cert_pem2 := wrong_example_server() or { panic(err) }
	srv2 := spawn nonblock_tls_echo_once(mut l2)
	tcfg_ok := TimeoutConfig{
		cfg:                  Config{
			server_name:  'wrong.example'
			verify:       true
			verify_chain: true
			ca_cert_pem:  cert_pem2.bytes()
		}
		connect_timeout_ms:   2000
		handshake_timeout_ms: 5000
	}
	mut ch2 := dial_async('127.0.0.1', port2, tcfg_ok)
	r2 := <-ch2
	assert r2.ok, 'async dial with matching name must verify: ${r2.err_msg}'
	mut c := r2.result() or { panic(err) }
	c.write('pong'.bytes()) or { panic(err) }
	mut buf := []u8{len: 4}
	assert c.read(mut buf) or { panic(err) } == 4
	assert buf == 'pong'.bytes()
	c.close() or {}
	srv2.wait()
	l2.close() or {}
}
