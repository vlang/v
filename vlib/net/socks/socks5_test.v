module socks

import net
import time

fn serve_greeting_then_close(mut l net.TcpListener) {
	mut conn := l.accept() or { return }
	mut greeting := []u8{len: 3}
	if conn.read(mut greeting) or { return } != 3 {
		return
	}
	// answer the method selection with 'no authentication, accepted'
	conn.write([u8(socks_version5), u8(0)]) or { return }
	// give the client time to consume the answer before the socket goes away
	time.sleep(100 * time.millisecond)
	conn.close() or {}
}

fn test_socks5_dial_rejects_ipv6_destination() ! {
	// the IPv6 branch was empty, so an IPv6 destination wrote no ATYP and no
	// address, producing '05 01 00 <port>' instead of a valid CONNECT request
	mut listener := net.listen_tcp(.ip, '127.0.0.1:0')!
	defer { listener.close() or {} }
	mut served := spawn serve_greeting_then_close(mut listener)
	proxy := listener.addr()!.str()
	if conn := socks5_dial(proxy, '::1:80', '', '') {
		assert false, 'an IPv6 destination must be rejected, not sent'
	} else {
		assert err.msg() == 'IPv6 SOCKS5 destinations are not supported'
	}
	served.wait()
}
