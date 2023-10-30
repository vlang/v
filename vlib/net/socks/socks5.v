module socks

import net.ssl
import net

const socks_version5 = u8(5)

const addr_type_ipv4 = u8(1)

const addr_type_fqdn = u8(3)

const addr_type_ipv6 = u8(4)

const no_auth = u8(1)

// socks5_dial create new instance of &net.TcpConn
pub fn socks5_dial(proxy_url string, host string) !&net.TcpConn {
	mut con := net.dial_tcp(proxy_url)!
	return handshake(mut con, host)!
}

// socks5_ssl_dial create new instance of &ssl.SSLConn
pub fn socks5_ssl_dial(proxy_url string, host string) !&ssl.SSLConn {
	mut ssl_conn := ssl.new_ssl_conn(
		verify: ''
		cert: ''
		cert_key: ''
		validate: false
		in_memory_verification: false
	)!
	mut con := socks5_dial(proxy_url, host)!
	ssl_conn.connect(mut con, host.all_before_last(':')) or { panic(err) }
	return ssl_conn
}

fn handshake(mut con net.TcpConn, host string) !&net.TcpConn {
	mut v := [socks.socks_version5, socks.no_auth, 0]

	con.write(v)!
	mut bf := []u8{len: 2}
	con.read(mut bf)!

	if bf[0] != socks.socks_version5 {
		con.close()!
		return error('unexpected protocol version ${bf[0]}')
	}

	if bf[1] != 0 {
		con.close()!
		return error(reply(bf[1]))
	}

	mut port := host.all_after_last(':').u64()
	if port == 0 {
		port = u64(80)
	}
	address := host.all_before_last(':')
	println(address)
	println(port)
	if address.contains_only('.1234567890') { // ipv4
		v << socks.addr_type_ipv4
		v << parse_ipv4(address)!
	} else if address.contains_only(':1234567890abcdf') {
		// v << addr_type_ipv6
		// v << parse_ipv4(address)!
		// todo support ipv6
	} else { // domain
		if address.len > 255 {
			return error('${address} is too long')
		} else {
			v << socks.addr_type_fqdn
			v << u8(address.len)
			v << address.bytes()
		}
	}
	v << u8(port >> 8)
	v << u8(port)

	con.write(v)!

	mut bff := []u8{len: 16}

	con.read(mut bff)!
	if bff[1] != 0 {
		con.close()!
		return error(reply(bff[1]))
	}
	return con
}

fn reply(code u8) string {
	match code {
		0 {
			return 'succeeded'
		}
		1 {
			return 'general SOCKS server failure'
		}
		2 {
			return 'connection not allowed by ruleset'
		}
		3 {
			return 'network unreachable'
		}
		4 {
			return 'host unreachable'
		}
		5 {
			return 'connection refused'
		}
		6 {
			return 'TTL expired'
		}
		7 {
			return 'command not supported'
		}
		8 {
			return 'address type not supported'
		}
		else {
			return 'unknown code: ${code}'
		}
	}
}

fn parse_ipv4(addr string) ![]u8 {
	mut ip := []u8{}
	for part in addr.split('.') {
		ip << part.u8()
	}
	// if ip.len != 4 {
	// 	return error('Ip address not valid')
	// }

	return ip
}
