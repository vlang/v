import net
import time

// last_write_sent must reflect the bytes the most recent write managed to send,
// both on success and (crucially) when the write fails, so a caller can tell a
// zero-byte failure from a partial write. See vlang/v#27459.

fn drain_one_conn(mut l net.TcpListener) {
	mut c := l.accept() or { return }
	for {
		mut buf := []u8{len: 4096}
		c.read(mut buf) or { break }
	}
	c.close() or {}
}

fn test_last_write_sent_on_success() {
	mut l := net.listen_tcp(.ip, '127.0.0.1:0')!
	addr := l.addr()!
	th := spawn drain_one_conn(mut l)
	mut c := net.dial_tcp(addr.str())!
	payload := 'hello world'.bytes()
	n := c.write(payload)!
	assert n == payload.len
	assert c.last_write_sent == payload.len
	c.close() or {}
	l.close() or {}
	th.wait()
}

fn test_last_write_sent_is_zero_on_failed_write_to_closed_conn() {
	mut l := net.listen_tcp(.ip, '127.0.0.1:0')!
	addr := l.addr()!
	th := spawn drain_one_conn(mut l)
	mut c := net.dial_tcp(addr.str())!
	// Close our side, then attempt a write: it must fail and report zero bytes
	// sent, so a retry layer can treat it as safe to replay.
	c.close() or {}
	time.sleep(5 * time.millisecond)
	c.write('data'.bytes()) or {
		assert c.last_write_sent == 0, 'a failed write before any byte left must report 0'
		l.close() or {}
		th.wait()
		return
	}
	assert false, 'write to a closed connection unexpectedly succeeded'
	l.close() or {}
	th.wait()
}
