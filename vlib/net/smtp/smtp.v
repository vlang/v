module smtp

/*
*
* smtp module 
* Created by: nedimf (07/2020)
*/
import net
import encoding.base64
import strings

struct Commands {
	ehlo       string = 'EHLO Alice\r\n'
	auth_plain string = 'AUTH PLAIN '
	mail_from  string = 'MAIL FROM:'
	recpt_to   string = 'RCPT TO:'
	data       string = 'DATA\r\n'
	subject    string = 'Subject:'
	end_msg    string = '\r\n.\r\n'
	quit       string = 'QUIT\r\n'
}

// Sends an email trough SMTP socket
pub fn send_mail(mailserver string, port int, username, password, subject, from, to, body, type_body string, debug bool) ?bool {
	client := connect(mailserver, port, debug)?
	send_ehlo(client, debug)
	auth(client, username, password, debug)
	send_mailfrom(client, from, debug)
	send_mailto(client, to, debug)
	send_data(client, debug)
	if type_body == 'html' {
		is_sent := send_html_body(client, subject, from, to, body, debug) or {
			return false
		}
		return is_sent
	} else {
		is_sent := send_text_body(client, subject, from, to, body, debug) or {
			return false
		}
		return is_sent
	}
	send_quit(client, debug)
}

// Creates socket connection with TCP server on provided port and returns net.Socket
pub fn connect(mailserver string, port int, debug bool) ?(net.Socket) {
	mut client := net.dial(mailserver, port)?
	bytes, blen := client.recv(1024)
	recv := recieved(bytes, blen)
	if recv.len >= 3 {
		is_debug(debug, recv)
		status := recv[..3]
		if status.int() != 220 {
			return error('Replay (220) from server has not been recieved.\nReplay recieved: $status')
		}
	} else {
		return error('Recieved data from SMTP server is not returning supported values\nReturned values: $recv')
	}
	return client
}

// Sends EHLO command to connected server. EHLO (Extended HELO) tells mailserver that our connection uses ESMTP
pub fn send_ehlo(socket net.Socket, debug bool) ?bool {
	cmd := Commands{}
	recv := send(socket, cmd.ehlo)
	if recv.len >= 3 {
		status := recv[..3]
		is_debug(debug, recv)
		if status.int() != 250 {
			return error('Replay (250) from server has not been recieved for EHLO.\nReplay recieved: $status')
		}
	} else {
		return error('Recieved data from SMTP server is not returning supported values\nReturned values: $recv')
	}
	return true
}

// Closing the connection with server
pub fn send_quit(socket net.Socket, debug bool) ?bool {
	cmd := Commands{}
	recv := send(socket, cmd.quit)
	if recv.len >= 3 {
		status := recv[..3]
		is_debug(debug, recv)
		if status.int() != 221 {
			return error('Replay (221) from server has not been recieved for QUIT.\nReplay recieved: $status')
		}
	} else {
		return error('Recieved data from SMTP server is not returning supported values\nReturned values: $recv')
	}
	return true
}

fn send_mailfrom(socket net.Socket, from string, debug bool) ?bool {
	cmd := Commands{}
	recv := send(socket, cmd.mail_from + ' <$from>\r\n')
	if recv.len >= 3 {
		status := recv[..3]
		is_debug(debug, recv)
		if status.int() != 221 {
			return error('Replay (221) from server has not been recieved for MAIL FROM.\nReplay recieved: $status')
		}
	} else {
		return error('Recieved data from SMTP server is not returning supported values\nReturned values: $recv')
	}
	return true
}

fn send_mailto(socket net.Socket, to string, debug bool) ?bool {
	cmd := Commands{}
	recv := send(socket, cmd.recpt_to + ' <$to>\r\n')
	if recv.len >= 3 {
		status := recv[..3]
		is_debug(debug, recv)
		if status.int() != 221 {
			return error('Replay (221) from server has not been recieved for MAIL TO.\nReplay recieved: $status')
		}
	} else {
		return error('Recieved data from SMTP server is not returning supported values\nReturned values: $recv')
	}
	return true
}

fn send_data(socket net.Socket, debug bool) ?bool {
	cmd := Commands{}
	recv := send(socket, cmd.data)
	if recv.len >= 3 {
		status := recv[..3]
		is_debug(debug, recv)
		if status.int() != 354 {
			return error('Replay (354) from server has not been recieved for DATA.\nReplay recieved: $status')
		}
	} else {
		return error('Recieved data from SMTP server is not returning supported values\nReturned values: $recv')
	}
	return true
}

fn send(socket net.Socket, string_to_send string) string {
	socket.send_string(string_to_send)
	bytes, blen := socket.recv(1024)
	return recieved(bytes, blen)
}

fn send_text_body(socket net.Socket, subject, from, to, body string, debug bool) ?bool {
	socket.send_string('From: $from\r\n')
	socket.send_string('To: $to\r\n')
	socket.send_string('Subject: $subject\r\n')
	socket.send_string('\r\n\r\n')
	socket.send_string(body)
	socket.send_string('\r\n.\r\n')
	bytes, blen := socket.recv(1024)
	recv := recieved(bytes, blen)
	if recv.len >= 3 {
		status := recv[..3]
		is_debug(debug, recv)
		if status.int() != 250 {
			return error('Replay (250) from server has not been recieved for EHLO.\nReplay recieved: $status')
		}
		return true
	} else {
		return error('Recieved data from SMTP server is not returning supported values\nReturned values: $recv')
	}
}

fn send_html_body(socket net.Socket, subject, from, to, body string, debug bool) ?bool {
	socket.send_string('From: $from\r\n')
	socket.send_string('To: $to\r\n')
	socket.send_string('Subject: $subject\r\n')
	socket.send_string('Content-Type: text/html; charset=ISO-8859-1')
	socket.send_string('\r\n\r\n')
	socket.send_string(body)
	socket.send_string('\r\n.\r\n')
	bytes, blen := socket.recv(1024)
	recv := recieved(bytes, blen)
	if recv.len >= 3 {
		is_debug(debug, recv)
		status := recv[..3]
		if status.int() != 250 {
			return error('Replay (250) from server has not been recieved for EHLO.\nReplay recieved: $status')
		}
		return true
	} else {
		return error('Recieved data from SMTP server is not returning supported values\nReturned values: $recv')
	}
}

fn auth(client net.Socket, username, password string, debug bool) ?bool {
	cmd := Commands{}
	mut sb := strings.new_builder(100)
	sb.write_b(0)
	sb.write(username)
	sb.write_b(0)
	sb.write(password)
	mut x := sb.str()
	x = base64.encode(x)
	auth_cmd := cmd.auth_plain + x + '\r\n'
	recv := send(client, auth_cmd)
	if recv.len >= 3 {
		is_debug(debug, recv)
		status := recv[..3]
		if status.int() != 235 {
			return error('Replay (235) from server has not been recieved for AUTH.\nReplay recieved: $status')
		}
	} else {
		return error('Recieved data from SMTP server is not returning supported values\nReturned values: $recv')
	}
	return true
}

fn recieved(bytes byteptr, blen int) string {
	return tos(bytes, blen)
}

fn is_debug(debug bool, pr string) {
	if debug == true {
		println('\n...\n')
		println(pr)
	}
}
