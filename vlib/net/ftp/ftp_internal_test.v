module ftp

import net

// test_get_host_ip_from_dtp_message_rejects_short_field_list checks that a
// PASV reply with fewer than six comma-separated fields does not panic.
// is_dtp_message_valid only checks for the presence of `(`, `)` and `,`, so
// e.g. `227 Entering Passive Mode (1,2)` reaches the parser.
// See https://github.com/vlang/v/issues/30176
fn test_get_host_ip_from_dtp_message_rejects_short_field_list() {
	ip, port := get_host_ip_from_dtp_message('227 Entering Passive Mode (1,2)')
	assert ip == '' && port == 0
	// the documented shape still parses
	host, pport := get_host_ip_from_dtp_message('227 Entering Passive Mode (209,132,183,61,48,218)')
	assert host == '209.132.183.61'
	assert pport == 48 * 256 + 218
}

// test_read_skips_short_continuation_lines checks that a blank line inside a
// multi-line reply does not panic the reader with an out-of-range slice.
// See https://github.com/vlang/v/issues/30176
fn test_read_skips_short_continuation_lines() {
	mut listener := net.listen_tcp(.ip, '127.0.0.1:0')!
	port := listener.addr()!.port()!
	spawn fn (mut listener net.TcpListener) {
		mut conn := listener.accept() or { return }
		conn.write_string('220-Welcome\r\n\r\n220 ready\r\n') or {}
		conn.close() or {}
	}(mut listener)
	mut zftp := new()
	assert zftp.connect('127.0.0.1:${port}')!
	zftp.close() or {}
}
