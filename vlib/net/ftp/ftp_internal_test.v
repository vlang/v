module ftp

import net
import time

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
	defer { listener.close() or {} }
	port := listener.addr()!.port()!
	mut sender := spawn fn (mut listener net.TcpListener) ! {
		mut conn := listener.accept()!
		defer { conn.close() or {} }
		conn.set_write_timeout(2 * time.second)
		conn.write_string('220-Welcome\r\n\r\n2\r\n22\r\n999 other\r\n220+ not finished\r\n220 ready\r\n')!
	}(mut listener)
	mut zftp := new()
	assert zftp.connect('127.0.0.1:${port}')!
	zftp.close() or {}
	sender.wait()!
}

fn test_pasv_rejects_malformed_or_out_of_range_fields() {
	for value in ['', '(1,2)', '(1,2,3,4,5)', '(1,2,3,4,5,6,7)', '(1,2,3,4,5,)', ')1,2,3,4,5,6(',
		'(1,2,3,4,5,6', '(1,2,3,4,5,256)', '(256,2,3,4,5,6)', '(-1,2,3,4,5,6)', '(a,2,3,4,5,6)',
		'(1,2,3,4,5,6x)', '(1,2,3,4,5,99999999999999999999)'] {
		ip, port := get_host_ip_from_dtp_message(value)
		assert ip == '' && port == 0, value
	}
	ip, port := get_host_ip_from_dtp_message('227 Entering Passive Mode (255,0,128,1,255,255)')
	assert ip == '255.0.128.1'
	assert port == 65535
	if _ := new_dtp('227 Entering Passive Mode (1,2)') {
		assert false, 'malformed passive reply must be rejected before dialing'
	} else {
		assert err.msg() == 'Bad message'
	}
}
