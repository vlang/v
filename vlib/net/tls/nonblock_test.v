// nonblock_test.v — tests for nonblock.v (timeout/async I/O).
//
// Every spawned server thread is joined with wait() before the test ends,
// so no threads leak between tests.
module tls

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
