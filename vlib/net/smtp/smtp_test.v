import net
import smtp

/*
*
* smtp_test
* Created by: nedimf (07/2020)
*/
fn test_smtp() {
	$if !network ? {
		return
	}
	server := 'smtp.mailtrap.io'
	port := 2525
	username := ''
	password := ''
	subject := 'Hello from V'
	from := 'developers@vlang.io'
	to := 'developers@vlang.io'
	msg := '<h1>Hi,from V module, this message was sent by SMTP!</h1>'
	body_type := 'html'
	debug := true // use while debugging
	// Test sending body_type = html
	is_sent_html := smtp.send_mail(server, port, username, password, subject, from, to,
		msg, body_type, debug) or {
		false
	}
	is_sent(is_sent_html)
	// Test sending body_type = text
	is_sent_text := smtp.send_mail(server, port, username, password, subject, from, to,
		msg, 'text', debug) or {
		false
	}
	is_sent(is_sent_text)
	// Test mailserver connection
	client := smtp.connect(server, port, debug) or {
		return
	}
	// Test socket connection created by sending ehlo command
	ehlo_test(client)
	// Test closing connection
	quit_test(client)
}

fn ehlo_test(socket net.Socket) {
	is_ehlo_success := smtp.send_ehlo(socket, true) or {
		false
	}
	if is_ehlo_success == true {
		assert true
		println('V: Ehlo was success')
	} else {
		println('V: Ehlo failed')
	}
}

fn quit_test(socket net.Socket) {
	is_quit_success := smtp.send_quit(socket, true) or {
		false
	}
	if is_quit_success == true {
		assert true
		println('V: Quit was success')
	} else {
		println('V: Quit failed')
	}
}

fn is_sent(sent bool) {
	if sent == true {
		assert true
		println('V: Email sent successfully')
	} else {
		println('V: Email failed to send.')
	}
}
