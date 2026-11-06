import net
import time

fn echo_server(mut c net.UdpConn, ready chan bool) {
	mut buf := []u8{len: 100}
	ready <- true
	read, addr := c.read(mut buf) or { panic('echo server could not read: ${err}') }
	eprintln('Server got addr ${addr}, read: ${read} | buf: ${buf}')
	c.write_to(addr, buf[..read]) or { panic('echo server could not write: ${err}') }
}

fn echo(server_addr string) ! {
	mut c := net.dial_udp(server_addr) or { panic('could not net.dial_udp: ${err}') }
	defer {
		c.close() or {}
	}
	c.set_read_timeout(time.second)
	data := 'Hello from vlib/net!'

	c.write_string(data) or { panic('could not write_string: ${err}') }

	mut buf := []u8{len: 100, init: 0}
	read, addr := c.read(mut buf) or { panic('could not read: ${err}') }

	assert read == data.len
	println('Got address ${addr}')
	// Can't test this here because loopback addresses
	// are mapped to other addresses
	// assert addr.str() == '127.0.0.1:30001'

	for i := 0; i < read; i++ {
		assert buf[i] == data[i]
	}

	println('Got "${buf.bytestr()}"')

	c.close()!
}

fn test_udp() {
	mut l := net.listen_udp('127.0.0.1:0') or { panic('could not listen_udp: ${err}') }
	defer {
		l.close() or {}
	}
	l.set_read_timeout(time.second)
	server_addr := l.sock.address() or { panic(err) }
	ready := chan bool{cap: 1}
	server := spawn echo_server(mut l, ready)
	defer {
		server.wait()
	}
	// Thread startup must not consume the client's read timeout.
	_ = <-ready
	echo(server_addr.str()) or { panic('could not echo: ${err}') }
}

fn udp_write_after_delay(addr string, delay time.Duration, payload string, done chan bool) {
	time.sleep(delay)
	mut conn := net.dial_udp(addr) or { panic(err) }
	defer {
		conn.close() or {}
	}
	conn.write_string(payload) or { panic(err) }
	done <- true
}

fn test_udp_read_timeout_is_honored_for_blocking_reads() ! {
	mut listener := net.listen_udp('127.0.0.1:0')!
	defer {
		listener.close() or {}
	}
	addr := listener.sock.address()!.str()
	delay := 200 * time.millisecond
	timeout := 20 * time.millisecond
	payload := 'late udp packet'
	done := chan bool{cap: 1}

	spawn udp_write_after_delay(addr, delay, payload, done)
	listener.set_read_timeout(timeout)

	mut buf := []u8{len: payload.len}
	if _, _ := listener.read(mut buf) {
		assert false, 'expected udp read timeout before delayed packet arrived'
	} else {
		assert err.code() == net.err_timed_out_code
	}

	_ = <-done
	listener.set_read_timeout(time.second)
	read, _ := listener.read(mut buf)!
	assert read == payload.len
	assert buf[..read].bytestr() == payload
}

fn test_udp_empty_datagram_reads_as_zero_bytes() ! {
	mut listener := net.listen_udp('127.0.0.1:0')!
	defer {
		listener.close() or {}
	}
	addr := listener.sock.address()!.str()
	mut sender := net.dial_udp(addr)!
	defer {
		sender.close() or {}
	}
	sender.write([]u8{})!
	sender.write_string('after')!
	listener.set_read_timeout(time.second)
	mut buf := []u8{len: 16}
	// an empty datagram is a datagram, not an error or the end of a stream
	n, from := listener.read(mut buf)!
	assert n == 0
	assert from.str().starts_with('127.0.0.1:')
	n2, _ := listener.read(mut buf)!
	assert buf[..n2].bytestr() == 'after'
}

fn test_udp_empty_datagram_arriving_during_the_read_is_zero_bytes() ! {
	mut listener := net.listen_udp('127.0.0.1:0')!
	defer {
		listener.close() or {}
	}
	addr := listener.sock.address()!.str()
	done := chan bool{cap: 1}
	// sent once the read is waiting, so a non-blocking socket takes its EAGAIN retry
	spawn udp_write_after_delay(addr, 50 * time.millisecond, '', done)
	listener.set_read_timeout(time.second)
	mut buf := []u8{len: 16}
	n, _ := listener.read(mut buf)!
	assert n == 0
	_ = <-done
}

fn test_udp_multicast_ipv4_socket_options() ! {
	mut listener := net.listen_udp('0.0.0.0:0')!
	defer {
		listener.close() or {}
	}

	listener.join_multicast_group('224.0.0.1', '0.0.0.0')!
	listener.leave_multicast_group('224.0.0.1', '0.0.0.0')!
	listener.set_multicast_ttl(2)!
	listener.set_multicast_loop(true)!
	listener.set_multicast_loop(false)!
	listener.set_multicast_interface('0.0.0.0')!
}

fn test_udp_multicast_validates_inputs() ! {
	mut listener := net.listen_udp('0.0.0.0:0')!
	defer {
		listener.close() or {}
	}

	mut ttl_failed := false
	listener.set_multicast_ttl(-1) or {
		ttl_failed = true
		assert err.msg().contains('multicast ttl')
	}
	assert ttl_failed

	mut non_multicast_failed := false
	listener.join_multicast_group('127.0.0.1', '') or {
		non_multicast_failed = true
		assert err.msg().contains('not an ipv4 multicast address')
	}
	assert non_multicast_failed

	mut iface_failed := false
	listener.set_multicast_interface('not-an-ip') or {
		iface_failed = true
		assert err.msg().contains('ipv4 multicast interface')
	}
	assert iface_failed
}

fn test_udp_read_deadline_wins_over_the_default_timeout() ! {
	mut listener := net.listen_udp('127.0.0.1:0')!
	defer {
		listener.close() or {}
	}
	// the default read timeout is 100 ms; a later deadline is what the read waits for
	listener.set_read_deadline(time.now().add(400 * time.millisecond))
	mut buf := []u8{len: 8}
	sw := time.new_stopwatch()
	if _, _ := listener.read(mut buf) {
		assert false, 'nothing was sent'
	} else {
		assert err.code() == net.err_timed_out_code
	}
	assert sw.elapsed() >= 350 * time.millisecond, 'returned after ${sw.elapsed()}'
}
