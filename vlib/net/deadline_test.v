import net
import time

fn tcp_write_after_deadline_delay(addr string) {
	time.sleep(100 * time.millisecond)
	mut client := net.dial_tcp(addr) or { panic(err) }
	defer {
		client.close() or {}
	}
	time.sleep(100 * time.millisecond)
	client.write_string('future') or { panic(err) }
}

fn test_far_future_tcp_deadlines_allow_delayed_readiness() ! {
	deadline := time.parse_iso8601('9999-12-31T23:59:59Z')!
	mut listener := net.listen_tcp(.ip, '127.0.0.1:0')!
	defer {
		listener.close() or {}
	}
	listener.set_accept_timeout(20 * time.millisecond)
	listener.set_accept_deadline(deadline)
	writer := spawn tcp_write_after_deadline_delay(listener.addr()!.str())
	mut server := listener.accept()!
	defer {
		server.close() or {}
	}
	server.set_read_timeout(20 * time.millisecond)
	server.set_read_deadline(deadline)
	mut buf := []u8{len: 16}
	read := server.read(mut buf)!
	writer.wait()
	assert buf[..read].bytestr() == 'future'
	assert listener.accept_deadline()! == deadline
	assert server.read_deadline()! == deadline
}

fn udp_write_after_deadline_delay(addr string, payload string) {
	time.sleep(200 * time.millisecond)
	mut sender := net.dial_udp(addr) or { panic(err) }
	defer {
		sender.close() or {}
	}
	sender.write_string(payload) or { panic(err) }
}

fn test_far_future_udp_deadlines_allow_delayed_readiness() ! {
	deadline := time.parse_iso8601('9999-12-31T23:59:59Z')!
	for use_default_timeout in [true, false] {
		mut listener := net.listen_udp('127.0.0.1:0')!
		defer {
			listener.close() or {}
		}
		if !use_default_timeout {
			listener.set_read_timeout(20 * time.millisecond)
		}
		listener.set_read_deadline(deadline)
		payload := 'future udp packet'
		writer := spawn udp_write_after_deadline_delay(listener.sock.address()!.str(), payload)
		mut buf := []u8{len: payload.len}
		read, _ := listener.read(mut buf)!
		writer.wait()
		assert buf[..read].bytestr() == payload
		assert listener.read_deadline()! == deadline
	}
}
