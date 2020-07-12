import smtp

fn test_smtp() {
	server := 'smtp.mailtrap.io'
	port := 2525
	username := ''
	password := ''
	subject := 'Hello from V'
	from := 'devlang.io'
	to := 'devlang@vlang.io,devlang@pop.com'
	msg := '<h1>Hi,from V module, this message was sent by SMTP!</h1>'
	body_type := 'html'
	debug := false // use while debugging
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
}

fn is_sent(sent bool) {
	if sent == true {
		println('V: Email sent successfully')
	} else {
		println('V: Email failed to send.')
	}
}
