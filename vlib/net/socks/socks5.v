module socks

import net.ssl
import net

const socks_version5 = u8(5)

const addr_type_ipv4 = u8(1)

const addr_type_fqdn = u8(3)

const addr_type_ipv6 = u8(4)

const no_auth = u8(0)

const auth_user_password = u8(2)

// socks5_dial create new instance of &net.TcpConn
pub fn socks5_dial(proxy_url string, host string, username string, password string) !&net.TcpConn {
	mut con := net.dial_tcp(proxy_url)!
	socks_conn_as_interface := handshake(mut con, host, username, password)!
	socks_conn := socks_conn_as_interface as net.TcpConn
	return &socks_conn
}

// socks5_ssl_dial create new instance of &ssl.SSLConn
pub fn socks5_ssl_dial(proxy_url string, host string, username string, password string) !&ssl.SSLConn {
	mut ssl_conn := ssl.new_ssl_conn(
		verify:                 ''
		cert:                   ''
		cert_key:               ''
		validate:               false
		in_memory_verification: false
	)!
	mut con := socks5_dial(proxy_url, host, username, password)!
	ssl_conn.connect(mut con, host.all_before_last(':')) or { panic(err) }
	return ssl_conn
}

// SOCKS5Dialer implements the Dialer interface initiating connections through a SOCKS5 proxy.
pub struct SOCKS5Dialer {
pub:
	dialer        net.Dialer
	proxy_address string
	username      string
	password      string
}

// new_socks5_dialer creates a dialer that will use a SOCKS5 proxy server to
// initiate connections. An underlying dialer is required to initiate the
// connection to the proxy server. Most users should use either
// net.default_tcp_dialer or ssl.create_ssl_dialer.
pub fn new_socks5_dialer(base net.Dialer, proxy_address string, username string, password string) net.Dialer {
	return &SOCKS5Dialer{
		dialer:        base
		proxy_address: proxy_address
		username:      username
		password:      password
	}
}

// dial initiates a new connection through the SOCKS5 proxy.
pub fn (sd SOCKS5Dialer) dial(address string) !net.Connection {
	mut conn := sd.dialer.dial(sd.proxy_address)!
	return handshake(mut conn, address, sd.username, sd.password)!
}

fn handshake(mut con net.Connection, host string, username string, password string) !net.Connection {
	mut v := [socks_version5, 1]
	if username.len > 0 {
		v << auth_user_password
	} else {
		v << no_auth
	}

	con.write(v)!
	mut bf := []u8{len: 2}
	con.read(mut bf)!

	if bf[0] != socks_version5 {
		con.close()!
		return error('unexpected protocol version ${bf[0]}')
	}
	if username.len == 0 {
		if bf[1] != 0 {
			con.close()!
			return error(reply(bf[1]))
		}
	}
	if username.len > 0 {
		v.clear()
		v << u8(1)
		v << u8(username.len)
		v << username.bytes()
		v << u8(password.len)
		v << password.bytes()

		con.write(v)!
		mut resp := []u8{len: 2}
		con.read(mut resp)!

		if resp[0] != 1 {
			con.close()!
			return error('server does not support user/password version 1')
		} else if resp[1] != 0 {
			con.close()!
			return error('user/password login failed')
		}
	}
	v.clear()
	v = [socks_version5, 1, 0]

	mut port := host.all_after_last(':').u64()
	if port == 0 {
		port = u64(80)
	}
	address := host.all_before_last(':')

	if address.contains_only('.1234567890') { // ipv4
		v << addr_type_ipv4
		v << parse_ipv4(address)!
	} else if address.contains_only(':1234567890abcdf') {
		// v << addr_type_ipv6
		// v << parse_ipv4(address)!
		// TODO: support ipv6
	} else { // domain
		if address.len > 255 {
			return error('${address} is too long')
		} else {
			v << addr_type_fqdn
			v << u8(address.len)
			v << address.bytes()
		}
	}
	v << u8(port >> 8)
	v << u8(port)

	con.write(v)!

	mut bff := []u8{len: v.len}

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

	return ip
}
