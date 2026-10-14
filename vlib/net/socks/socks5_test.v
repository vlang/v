module socks

import net

struct Socks5TestConnection {
mut:
	writes [][]u8
	closed bool
}

fn (c &Socks5TestConnection) addr() !net.Addr { return error('unused test address') }

fn (c &Socks5TestConnection) peer_addr() !net.Addr { return error('unused test address') }

fn (mut c Socks5TestConnection) read(mut buf []u8) !int {
	assert buf.len >= 2
	buf[0] = socks_version5
	buf[1] = no_auth
	return 2
}

fn (mut c Socks5TestConnection) write(data []u8) !int {
	c.writes << data.clone()
	return data.len
}

fn (mut c Socks5TestConnection) close() ! { c.closed = true }

fn test_socks5_handshake_rejects_ipv6_and_closes_connection() {
	for host in ['::1:80', '[::1]:80', '2001:db8::dead:BEEF:80', '[fe80::1%en0]:80'] {
		mut conn := &Socks5TestConnection{}
		if _ := handshake(mut conn, host, '', '') {
			assert false, 'IPv6 destinations must be rejected'
		} else {
			assert err.msg() == 'IPv6 SOCKS5 destinations are not supported'
		}
		assert conn.closed, 'a failed IPv6 handshake must close the proxy connection'
		assert conn.writes == [[socks_version5, u8(1), no_auth]], 'only the greeting may be written'
	}
}

fn test_socks5_handshake_hexadecimal_domains_remain_domains() ! {
	for domain in ['cafe', 'abc', 'deadbeef'] {
		mut conn := &Socks5TestConnection{}
		_ := handshake(mut conn, '${domain}:80', '', '')!
		mut request := [socks_version5, u8(1), u8(0), addr_type_fqdn, u8(domain.len)]
		request << domain.bytes()
		request << [u8(0), u8(80)]
		assert conn.writes.len == 2
		assert conn.writes[1] == request
		assert !conn.closed
	}
}

fn test_parse_ipv4() {
	assert parse_ipv4('255.255.255.255')! == [u8(255), 255, 255, 255]
	assert parse_ipv4('127.0.0.1')! == [u8(127), 0, 0, 1]
	parse_ipv4('1.2..2') or { assert err.msg() == 'Ip address not valid' }
}
