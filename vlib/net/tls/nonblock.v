// nonblock.v — timeout-aware dials and async dial for net.tls.
//
// This file only *uses* the existing Conn/Config/dial/listen API; it does
// not modify conn.v, client.v, server.v or tls.v. Timeouts are enforced with
// the socket primitives vlib/net already exposes on net.TcpConn
// (set_read_timeout / set_write_timeout / set_read_deadline /
// set_write_deadline), so this file stays pure V (-Wimpure-v clean).
module tls

import net
import time

// Timeouts groups millisecond timeouts for one TLS connection attempt.
// A zero value means "no timeout": the blocking default. The TCP connect
// itself is still bounded by the OS (about 5 seconds) when
// connect_timeout_ms is 0.
pub struct Timeouts {
pub mut:
	connect_timeout_ms   int
	handshake_timeout_ms int
	read_timeout_ms      int
	write_timeout_ms     int
}

// default_timeouts returns a starting point: bounded connect and handshake
// phases, blocking application reads and writes.
pub fn default_timeouts() Timeouts {
	return Timeouts{
		connect_timeout_ms:   5000
		handshake_timeout_ms: 10000
	}
}

// TimeoutConfig pairs a base TLS Config with timeouts. It is the
// Config-compatible entry point for timeout-aware dials: fill cfg exactly
// as for dial(), then set whichever millisecond fields you need.
pub struct TimeoutConfig {
pub mut:
	cfg Config
	// connect_timeout_ms bounds the TCP dial. Values above the OS limit
	// (about 5 seconds) still fail at the OS limit: this bound can only
	// shorten the wait, never extend it.
	connect_timeout_ms int = 5000
	// handshake_timeout_ms bounds the whole TLS handshake via absolute
	// socket deadlines around handshake().
	handshake_timeout_ms int = 10000
	// read_timeout_ms / write_timeout_ms become the steady-state socket
	// timeouts of the live Conn (0 = blocking default).
	read_timeout_ms  int
	write_timeout_ms int
	// cancel, when set, cooperatively cancels an async dial: close the
	// channel to cancel. It is checked before dialing, after the TCP
	// connect, and after the handshake. A handshake already blocked in a
	// socket read still runs until handshake_timeout_ms (or unbounded
	// when that is 0), so always set a handshake timeout when you plan
	// to cancel.
	cancel ?chan bool
}

// timeouts collapses the TimeoutConfig timeout fields into a Timeouts value.
pub fn (t TimeoutConfig) timeouts() Timeouts {
	return Timeouts{
		connect_timeout_ms:   t.connect_timeout_ms
		handshake_timeout_ms: t.handshake_timeout_ms
		read_timeout_ms:      t.read_timeout_ms
		write_timeout_ms:     t.write_timeout_ms
	}
}

// DialResult is the single value delivered by dial_async.
pub struct DialResult {
pub mut:
	conn &Conn = unsafe { nil }
	ok   bool
	// err_msg / err_code describe the failure when ok is false.
	err_msg  string
	err_code int
	// timed_out is set when the failure was a timeout
	// (err_code == net.err_timed_out_code).
	timed_out bool
	// cancelled is set when tcfg.cancel was closed before completion.
	cancelled bool
}

// result returns the connected Conn, or the dial failure as an error.
pub fn (r DialResult) result() !&Conn {
	if r.ok && r.conn != unsafe { nil } {
		return r.conn
	}
	if r.cancelled {
		return error('tls: async dial cancelled')
	}
	if r.timed_out {
		return error_with_code('tls: async dial timed out: ${r.err_msg}', net.err_timed_out_code)
	}
	return error_with_code('tls: async dial failed: ${r.err_msg}', r.err_code)
}

// is_timeout reports whether err is a network timeout
// (err.code() == net.err_timed_out_code). Timeouts raised by the helpers
// in this file preserve that code through handshake and dial failures.
pub fn is_timeout(err IError) bool {
	return err.code() == net.err_timed_out_code
}

// apply_timeouts sets the steady-state socket timeouts on a live Conn.
// Positive values are applied via net.TcpConn.set_read_timeout /
// set_write_timeout; zero restores the blocking default (net.no_timeout).
// Note this intentionally bypasses Conn.set_read_timeout, which is a
// no-op placeholder kept for net.ssl compatibility.
pub fn apply_timeouts(mut c Conn, t Timeouts) {
	if t.read_timeout_ms > 0 {
		c.tcp.set_read_timeout(time.Duration(t.read_timeout_ms) * time.millisecond)
	} else {
		c.tcp.set_read_timeout(net.no_timeout)
	}
	if t.write_timeout_ms > 0 {
		c.tcp.set_write_timeout(time.Duration(t.write_timeout_ms) * time.millisecond)
	} else {
		c.tcp.set_write_timeout(net.no_timeout)
	}
}

// set_conn_timeouts is a shorthand for apply_timeouts with only the
// steady-state read/write timeouts (milliseconds, 0 = blocking).
pub fn set_conn_timeouts(mut c Conn, read_timeout_ms int, write_timeout_ms int) {
	apply_timeouts(mut c, Timeouts{
		read_timeout_ms:  read_timeout_ms
		write_timeout_ms: write_timeout_ms
	})
}

// conn_read_timeout returns the live read timeout of c.
pub fn conn_read_timeout(c &Conn) time.Duration {
	return c.tcp.read_timeout()
}

// conn_write_timeout returns the live write timeout of c.
pub fn conn_write_timeout(c &Conn) time.Duration {
	return c.tcp.write_timeout()
}

// dial_with_timeouts dials host:port and runs the TLS handshake with the
// given timeouts. The handshake phase is bounded by absolute socket
// deadlines (handshake_timeout_ms); the steady-state read/write timeouts
// are applied to the returned Conn. Any failure closes the half-open
// connection before returning.
pub fn dial_with_timeouts(host string, port int, cfg Config, t Timeouts) !&Conn {
	tcp := dial_tcp_bounded('${host}:${port}', t.connect_timeout_ms)!
	mut ccfg := cfg
	// An empty server_name means "verify against the dialed host": without
	// this default any valid chain (for any name) would be accepted.
	// Mirrors dial() so timeout and async (delegating) paths verify identically.
	if ccfg.server_name == '' {
		ccfg.server_name = host
	}
	mut c := &Conn{
		cfg: ccfg
		tcp: tcp
	}
	if t.handshake_timeout_ms > 0 {
		dl := time.now().add(time.Duration(t.handshake_timeout_ms) * time.millisecond)
		c.tcp.set_read_deadline(dl)
		c.tcp.set_write_deadline(dl)
	}
	c.handshake() or {
		e := err
		c.close() or {}
		if e.code() == net.err_timed_out_code {
			return error_with_code('tls: handshake timed out after ${t.handshake_timeout_ms}ms',
				net.err_timed_out_code)
		}
		return e
	}
	c.tcp.set_read_deadline(time.Time{})
	c.tcp.set_write_deadline(time.Time{})
	apply_timeouts(mut c, t)
	return c
}

// dial_async runs dial_with_timeouts in the background and delivers exactly
// one DialResult on the returned (buffered) channel. The caller receives
// with `r := <-ch`. To cancel, close tcfg.cancel: the worker notices
// between phases and reports a cancelled DialResult. Do not close the
// result channel itself; the worker always sends exactly once and exits,
// so receiving the result implies no thread is left behind.
pub fn dial_async(host string, port int, tcfg TimeoutConfig) chan DialResult {
	res := chan DialResult{cap: 1}
	spawn async_dial_worker(host, port, tcfg, res)
	return res
}

// async_dial_worker is the thread behind dial_async.
fn async_dial_worker(host string, port int, tcfg TimeoutConfig, res chan DialResult) {
	if is_cancelled(tcfg) {
		res <- DialResult{
			err_msg:   'tls: dial cancelled before connect'
			cancelled: true
		}
		return
	}
	mut c := dial_with_timeouts(host, port, tcfg.cfg, tcfg.timeouts()) or {
		e := err
		res <- DialResult{
			err_msg:   e.msg()
			err_code:  e.code()
			timed_out: e.code() == net.err_timed_out_code
			cancelled: is_cancelled(tcfg)
		}
		return
	}
	if is_cancelled(tcfg) {
		c.close() or {}
		res <- DialResult{
			err_msg:   'tls: dial cancelled after handshake'
			cancelled: true
		}
		return
	}
	res <- DialResult{
		conn: c
		ok:   true
	}
}

// is_cancelled reports whether tcfg.cancel was set and closed.
fn is_cancelled(tcfg TimeoutConfig) bool {
	if cancel := tcfg.cancel {
		select {
			_ := <-cancel {
				return true
			}
			else {
			}
		}
	}
	return false
}

// TcpDialOutcome carries a threaded net.dial_tcp result back to the waiter.
struct TcpDialOutcome {
mut:
	conn     &net.TcpConn = unsafe { nil }
	err_msg  string
	err_code int
}

// dial_tcp_bounded dials address, waiting at most connect_timeout_ms.
// With a non-positive timeout it dials directly (bounded only by the OS).
// A late connect that finishes after the timeout is closed by a reaper
// thread so no socket leaks.
fn dial_tcp_bounded(address string, connect_timeout_ms int) !&net.TcpConn {
	if connect_timeout_ms <= 0 {
		return net.dial_tcp(address)!
	}
	mut out := &TcpDialOutcome{}
	done := chan bool{cap: 1}
	spawn tcp_dial_worker(address, mut out, done)
	select {
		_ := <-done {
			if out.conn == unsafe { nil } {
				return error_with_code('tls: tcp dial failed: ${out.err_msg}', out.err_code)
			}
			return out.conn
		}
		time.Duration(connect_timeout_ms) * time.millisecond {
			spawn tcp_dial_reaper(done, out)
			return error_with_code('tls: tcp dial timed out after ${connect_timeout_ms}ms',
				net.err_timed_out_code)
		}
	}
	return error('tls: unreachable')
}

// tcp_dial_worker performs the blocking net.dial_tcp for dial_tcp_bounded.
fn tcp_dial_worker(address string, mut out &TcpDialOutcome, done chan bool) {
	tcp := net.dial_tcp(address) or {
		out.err_msg = err.msg()
		out.err_code = err.code()
		done <- true
		return
	}
	out.conn = tcp
	done <- true
}

// tcp_dial_reaper owns a connect that finished after dial_tcp_bounded gave
// up: it waits for the worker, then closes the late connection (if any).
fn tcp_dial_reaper(done chan bool, out &TcpDialOutcome) {
	_ := <-done
	if out.conn != unsafe { nil } {
		mut late := out.conn
		late.close() or {}
	}
}
