module http

import io
import net
import time

fn test_idle_probe_does_not_consume_pending_bytes() {
	mut listener := net.listen_tcp(.ip, '127.0.0.1:0')!
	defer {
		listener.close() or {}
	}
	mut client := net.dial_tcp(listener.addr()!.str())!
	defer {
		client.close() or {}
	}
	mut server := listener.accept()!
	mut server_closed := false
	defer {
		if !server_closed {
			server.close() or {}
		}
	}
	pooled := &H1PooledConn{
		tcp: client
	}
	assert !pooled.idle_tcp_closed()
	server.write_string('pending')!
	client.set_read_timeout(time.second)
	client.wait_for_read()!
	assert !pooled.idle_tcp_closed()
	mut buf := []u8{len: 1}
	mut received := []u8{}
	for _ in 0 .. 7 {
		assert client.read(mut buf)! == 1
		received << buf[0]
	}
	assert received.bytestr() == 'pending'
	server.close()!
	server_closed = true
	if _ := client.read(mut buf) {
		assert false, 'expected EOF after the peer closed'
	} else {
		assert err is io.Eof
	}
	assert pooled.idle_tcp_closed()
}

fn wait_idle_reaper_stopped(mut transport Transport) {
	deadline := time.now().add(3 * time.second)
	for time.now() < deadline {
		transport.mu.lock()
		running := transport.idle_reaper_running
		transport.mu.unlock()
		if !running {
			return
		}
		time.sleep(time.millisecond)
	}
	assert false, 'idle reaper did not stop after draining the pool'
}

fn test_idle_connections_expire_without_another_request() {
	mut listener := net.listen_tcp(.ip, '127.0.0.1:0')!
	defer { listener.close() or {} }
	mut transport := new_transport()
	transport.idle_timeout = 20 * time.millisecond
	mut servers := []&net.TcpConn{}
	for key in ['first origin', 'second origin'] {
		client := net.dial_tcp(listener.addr()!.str())!
		mut server := listener.accept()!
		server.set_read_timeout(3 * time.second)
		servers << server
		mut pooled := &H1PooledConn{
			key: key
			tcp: client
		}
		transport.checkin(mut pooled)
	}
	defer { transport.close_idle() }
	// No checkout or request runs after checkin. EOF proves the descriptors closed.
	mut buf := []u8{len: 1}
	for mut server in servers {
		if _ := server.read(mut buf) {
			assert false, 'expected EOF after the idle timeout'
		} else {
			assert err is io.Eof
		}
		server.close()!
	}
	wait_idle_reaper_stopped(mut transport)
	transport.mu.lock()
	assert transport.h1_idle.len == 0
	transport.mu.unlock()
}

fn test_idle_reaper_preserves_checked_out_connections_and_can_restart() {
	mut listener := net.listen_tcp(.ip, '127.0.0.1:0')!
	defer { listener.close() or {} }
	mut transport := new_transport()
	transport.idle_timeout = 20 * time.millisecond
	client := net.dial_tcp(listener.addr()!.str())!
	mut server := listener.accept()!
	defer { server.close() or {} }
	mut pooled := &H1PooledConn{
		key: 'active'
		tcp: client
	}
	transport.checkin(mut pooled)
	mut active := transport.checkout('active')
	assert active != unsafe { nil }
	wait_idle_reaper_stopped(mut transport)
	server.write_string('still open')!
	active.tcp.set_read_timeout(time.second)
	mut buf := []u8{len: 10}
	assert active.tcp.read(mut buf)! == 10
	assert buf.bytestr() == 'still open'
	transport.checkin(mut active)
	server.set_read_timeout(3 * time.second)
	if _ := server.read(mut buf) {
		assert false, 'expected EOF after the restarted reaper expires the connection'
	} else {
		assert err is io.Eof
	}
	wait_idle_reaper_stopped(mut transport)
}

struct IdleReaperH2Transport {}

fn (mut t IdleReaperH2Transport) read(mut _buf []u8) !int {
	return error('idle reaper test does not perform network I/O')
}

fn (mut t IdleReaperH2Transport) write(_buf []u8) !int {
	return error('idle reaper test does not perform network I/O')
}

fn test_idle_reaper_expires_h2_pool_entries_and_preserves_active_streams() {
	mut transport := new_transport()
	transport.idle_timeout = 20 * time.millisecond
	mut expired := &H2MuxConn{
		transport:  IdleReaperH2Transport{}
		idle_since: time.now().add(-time.hour)
	}
	mut active := &H2MuxConn{
		transport:      IdleReaperH2Transport{}
		idle_since:     time.now().add(-time.hour)
		active_streams: 1
	}
	transport.mu.lock()
	transport.h2_conns['expired'] = expired
	transport.h2_dial_id['expired'] = 1
	transport.h2_conns['active'] = active
	transport.h2_dial_id['active'] = 2
	transport.start_idle_reaper_locked()
	transport.mu.unlock()
	deadline := time.now().add(3 * time.second)
	mut removed := false
	for time.now() < deadline {
		transport.mu.lock()
		removed = 'expired' !in transport.h2_conns
		assert 'active' in transport.h2_conns
		transport.mu.unlock()
		if removed {
			break
		}
		time.sleep(time.millisecond)
	}
	assert removed
	active.smu.lock()
	assert !active.shutting_down
	assert active.refs == 1
	active.active_streams = 0
	active.smu.unlock()
	wait_idle_reaper_stopped(mut transport)
	expired.smu.lock()
	assert expired.shutting_down
	assert expired.refs == 0
	assert expired.transport_torn_down
	expired.smu.unlock()
	active.smu.lock()
	assert active.shutting_down
	assert active.refs == 0
	assert active.transport_torn_down
	active.smu.unlock()
	transport.mu.lock()
	assert transport.h2_dial_id.len == 0
	transport.mu.unlock()
}
