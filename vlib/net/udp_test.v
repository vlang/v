import net
import time

fn echo_server(mut c net.UdpConn) {
	mut count := 0
	for {
		eprintln('> echo_server loop count: ${count}')
		mut buf := []u8{len: 100}
		read, addr := c.read(mut buf) or { continue }
		eprintln('Server got addr ${addr}, read: ${read} | buf: ${buf}')
		c.write_to(addr, buf[..read]) or {
			println('Server: connection dropped')
			return
		}
		count++
		// Normally, after responding, the test will end, but there are sometimes  cases,
		// when the echo_server continued looping, printing messages constantly.
		// The sleep here, is a small precaution against spamming the CI with log messages,
		// when there are network problems, and it allows easier root cause diagnostic, when
		// they do happen:
		time.sleep(1000 * time.millisecond)
	}
}

const server_addr = '127.0.0.1:40003'

fn echo() ! {
	mut c := net.dial_udp(server_addr) or { panic('could not net.dial_udp: ${err}') }
	defer {
		c.close() or {}
	}
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
	mut l := net.listen_udp(server_addr) or { panic('could not listen_udp: ${err}') }

	spawn echo_server(mut l)
	echo() or { panic('could not echo: ${err}') }

	l.close() or {}
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
