module ftp

import io
import net
import time

fn ftp_test_pair(lines []string) !(&FTP, &net.TcpListener) {
	mut listener := net.listen_tcp(.ip, '127.0.0.1:0')!
	mut conn := net.dial_tcp(listener.addr()!.str())!
	mut served := listener.accept()!
	for line in lines {
		served.write('${line}\r\n'.bytes()) or { return err }
	}
	served.set_read_timeout(2 * time.second)
	conn.set_read_timeout(2 * time.second)
	mut zftp := new()
	zftp.conn = conn
	zftp.reader = io.new_buffered_reader(reader: conn)
	return zftp, unsafe { &listener }
}

fn test_read_short_continuation_line_does_not_panic() ! {
	// a blank continuation line of a multi line reply used to index data[3]
	// on an empty string and panic
	mut zftp, mut listener := ftp_test_pair(['220-Welcome', '', '220 ready'])!
	defer { listener.close() or {} }
	code, _ := zftp.read()!
	assert code == 0
}

fn test_read_continuation_line_of_three_bytes_does_not_panic() ! {
	mut zftp, mut listener := ftp_test_pair(['220-Welcome', '220', '220 ready'])!
	defer { listener.close() or {} }
	code, _ := zftp.read()!
	assert code == 0
}

fn test_read_complete_multiline_reply() ! {
	// a well formed multi line reply still parses to its code
	mut zftp, mut listener := ftp_test_pair(['220-Welcome', '220 ready', '220 done'])!
	defer { listener.close() or {} }
	code, data := zftp.read()!
	assert code == 220
	assert data == '220 ready'
}

fn test_dtp_message_valid_reply() ! {
	dtp := new_dtp('227 Entering Passive Mode (209,132,183,61,48,218)')!
	assert dtp.ip == '209.132.183.61'
	assert dtp.port == 48 * 256 + 218
}

fn test_dtp_message_with_too_few_fields_is_rejected() ! {
	// '(1,2)' passes the old contains-based check, then data[0..4] panicked
	if _ := new_dtp('227 Entering Passive Mode (1,2)') {
		assert false, 'a PASV reply with too few fields must be rejected'
	} else {
		assert err.msg() == 'Bad message'
	}
}

fn test_dtp_message_with_reversed_parens_is_rejected() ! {
	// the old scan kept the last '(' and the last ')', so start > end here
	if _ := new_dtp('227 ) , (') {
		assert false, 'a PASV reply with the parens reversed must be rejected'
	} else {
		assert err.msg() == 'Bad message'
	}
}
