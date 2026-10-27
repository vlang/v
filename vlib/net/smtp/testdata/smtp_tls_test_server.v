module main

import net
import net.openssl
import os
import time

fn read_line_tcp(mut conn net.TcpConn) []u8 {
	mut data := []u8{}
	mut buf := []u8{len: 1}
	for {
		n := conn.read(mut buf) or { break }
		if n == 0 {
			break
		}
		data << buf[0]
		if data.len >= 2 && data[data.len - 2] == u8(13) && data[data.len - 1] == u8(10) {
			break
		}
	}
	return data
}

fn read_line_ssl(mut conn openssl.SSLConn) []u8 {
	mut data := []u8{}
	mut buf := []u8{len: 1}
	for {
		n := conn.read(mut buf) or { break }
		if n == 0 {
			break
		}
		data << buf[0]
		if data.len >= 2 && data[data.len - 2] == u8(13) && data[data.len - 1] == u8(10) {
			break
		}
	}
	return data
}

fn record(path string, value string) {
	os.write_file(path, value) or {}
}

fn record_no_auth(path string) {
	if !os.exists(path) {
		record(path, 'NO_AUTH')
	}
}

fn main() {
	if os.args.len < 7 {
		eprintln('usage: smtp_tls_test_server <mode> <host> <cert> <key> <ready_path> <result_path>')
		exit(1)
	}
	mode := os.args[1]
	host := os.args[2]
	cert := os.args[3]
	key := os.args[4]
	ready_path := os.args[5]
	result_path := os.args[6]

	// listen
	mut listener := net.listen_tcp(.ip, '${host}:0', net.ListenOptions{}) or {
		eprintln('listen failed: ${err}')
		exit(1)
	}
	listener.set_accept_timeout(10 * time.second)
	addr := listener.addr() or {
		eprintln('addr failed: ${err}')
		exit(1)
	}
	port := addr.port() or {
		eprintln('port failed: ${err}')
		exit(1)
	}
	// The client reads the port as soon as the ready file appears.
	temporary_ready := '${ready_path}.tmp'
	os.write_file(temporary_ready, port.str()) or {
		eprintln('ready write failed: ${err}')
		exit(1)
	}
	os.rename(temporary_ready, ready_path) or {
		eprintln('ready rename failed: ${err}')
		exit(1)
	}

	config := openssl.SSLConnectConfig{
		cert:     cert
		cert_key: key
	}

	mut tcp_conn := listener.accept() or {
		eprintln('accept failed: ${err}')
		exit(1)
	}
	tcp_conn.set_read_timeout(5 * time.second)
	tcp_conn.set_write_timeout(5 * time.second)

	if mode == 'starttls' {
		tcp_conn.write_string('220 localhost ESMTP ready\r\n') or {}
		line := read_line_tcp(mut tcp_conn)
		if !line.bytestr().starts_with('EHLO ') { // EHLO
			record(result_path, 'ERROR: expected EHLO before STARTTLS')
			return
		}
		tcp_conn.write_string('250-localhost\r\n250-STARTTLS\r\n250 OK\r\n') or {}
		line2 := read_line_tcp(mut tcp_conn)
		if line2.bytestr().trim_space().to_upper() != 'STARTTLS' {
			record(result_path, 'ERROR: expected STARTTLS')
			return
		}
		tcp_conn.write_string('220 Ready to start TLS\r\n') or {}
		// upgrade
		mut tls_conn := openssl.new_ssl_server_conn(mut tcp_conn, config) or {
			record_no_auth(result_path)
			return
		}
		// after upgrade, tls_conn owns ssl; tcp_conn still owns socket but we won't use it
		defer { tls_conn.shutdown() or {} }
		auth := read_line_ssl(mut tls_conn)
		if auth.len == 0 {
			record(result_path, 'NO_AUTH')
			return
		}
		if !auth.bytestr().starts_with('AUTH') { // AUTH
			record(result_path, 'UNEXPECTED: ${auth.hex()}')
			return
		}
		record(result_path, 'AUTH')
		if auth.bytestr().starts_with('AUTH PLAIN ') {
			tls_conn.write_string('235 Authentication successful\r\n') or {}
			q := read_line_ssl(mut tls_conn)
			if q.bytestr().trim_space().to_upper() == 'QUIT' {
				tls_conn.write_string('221 Bye\r\n') or {}
			}
		}
	} else {
		// implicit
		mut tls_conn := openssl.new_ssl_server_conn(mut tcp_conn, config) or {
			record_no_auth(result_path)
			return
		}
		defer { tls_conn.shutdown() or {} }
		tls_conn.write_string('220 localhost ESMTP ready\r\n') or {}
		line := read_line_ssl(mut tls_conn)
		if !line.bytestr().starts_with('EHLO ') {
			record_no_auth(result_path)
			return
		}
		tls_conn.write_string('250-localhost\r\n250 OK\r\n') or {}
		auth := read_line_ssl(mut tls_conn)
		if auth.len == 0 {
			record(result_path, 'NO_AUTH')
			return
		}
		if !auth.bytestr().starts_with('AUTH') {
			record(result_path, 'UNEXPECTED: ${auth.hex()}')
			return
		}
		record(result_path, 'AUTH')
		if auth.bytestr().starts_with('AUTH PLAIN ') {
			tls_conn.write_string('235 Authentication successful\r\n') or {}
			q := read_line_ssl(mut tls_conn)
			if q.bytestr().trim_space().to_upper() == 'QUIT' {
				tls_conn.write_string('221 Bye\r\n') or {}
			}
		}
	}
	listener.close() or {}
}
