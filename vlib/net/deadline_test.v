import net
import time

const deadline_fallback_timeout = 20 * time.millisecond
const deadline_delay_margin = 20 * time.millisecond

fn tcp_write_after_deadline_delay(addr string, read_ready chan bool) ! {
	time.sleep(deadline_fallback_timeout + deadline_delay_margin)
	mut client := net.dial_tcp(addr)!
	defer {
		client.close() or {}
	}
	select {
		_ := <-read_ready {
		}
		2 * time.second {
			return error('TCP reader did not become ready')
		}
	}
	time.sleep(deadline_fallback_timeout + deadline_delay_margin)
	client.write_string('future')!
}

fn test_far_future_tcp_deadlines_allow_delayed_readiness() ! {
	$if is_coroutine ? {
		return
	}
	deadline := time.parse_iso8601('9999-12-31T23:59:59Z')!
	mut listener := net.listen_tcp(.ip, '127.0.0.1:0')!
	defer {
		listener.close() or {}
	}
	listener.set_accept_timeout(deadline_fallback_timeout)
	listener.set_accept_deadline(deadline)
	read_ready := chan bool{cap: 1}
	writer := spawn tcp_write_after_deadline_delay(listener.addr()!.str(), read_ready)
	mut server := listener.accept()!
	defer {
		server.close() or {}
	}
	server.set_read_timeout(deadline_fallback_timeout)
	server.set_read_deadline(deadline)
	mut buf := []u8{len: 16}
	read_ready <- true
	read := server.read(mut buf)!
	writer.wait()!
	assert buf[..read].bytestr() == 'future'
	assert listener.accept_deadline()! == deadline
	assert server.read_deadline()! == deadline
}

fn udp_write_after_deadline_delay(addr string, payload string, delay time.Duration) ! {
	time.sleep(delay)
	mut sender := net.dial_udp(addr)!
	defer {
		sender.close() or {}
	}
	sender.write_string(payload)!
}

fn test_far_future_udp_deadlines_allow_delayed_readiness() ! {
	deadline := time.parse_iso8601('9999-12-31T23:59:59Z')!
	for use_default_timeout in [true, false] {
		mut listener := net.listen_udp('127.0.0.1:0')!
		defer {
			listener.close() or {}
		}
		if !use_default_timeout {
			listener.set_read_timeout(deadline_fallback_timeout)
		}
		listener.set_read_deadline(deadline)
		payload := 'future udp packet'
		mut buf := []u8{len: payload.len}
		// Deliver the packet after the configured timeout would have expired.
		delay := listener.read_timeout() + deadline_delay_margin
		writer := spawn udp_write_after_deadline_delay(listener.sock.address()!.str(), payload, delay)
		read, _ := listener.read(mut buf)!
		writer.wait()!
		assert buf[..read].bytestr() == payload
		assert listener.read_deadline()! == deadline
	}
}
