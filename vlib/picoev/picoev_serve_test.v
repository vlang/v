// vtest build: !windows && !solaris
import net
import net.http
import picoev
import picohttpparser
import time

fn callback(_ voidptr, _ picohttpparser.Request, mut res picohttpparser.Response) {
	res.http_ok()
	res.plain()
	res.body('served')
	res.end()
}

fn free_port() int {
	mut listener := net.listen_tcp(.ip, '127.0.0.1:0') or { panic(err) }
	port := (listener.addr() or { panic(err) }).port() or { panic(err) }
	listener.close() or {}
	return int(port)
}

// serves_requests starts a server of the given family, and expects answers on its loopback
// address. Several requests in a row make the loop apply the pending changes of several
// file descriptors, where a `next_fd` of -1 marks the end of their list.
fn serves_requests(family net.AddrFamily, host string) {
	port := free_port()
	mut pv := picoev.new(port: port, family: family, cb: callback) or { panic(err) }
	spawn pv.serve()
	time.sleep(100 * time.millisecond)
	for _ in 0 .. 3 {
		res := http.get('http://${host}:${port}/') or { panic(err) }
		assert res.status_code == 200
		assert res.body == 'served'
	}
}

fn test_serves_requests_over_ipv4() {
	serves_requests(.ip, '127.0.0.1')
}

fn test_serves_requests_on_the_default_dual_stack_socket() {
	serves_requests(.ip6, '127.0.0.1')
	serves_requests(.ip6, '[::1]')
}
