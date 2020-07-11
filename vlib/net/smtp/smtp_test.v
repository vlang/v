import smtp

fn test_smtp() {

	server := 'smtp.mailtrap.io'
	port := 2525
	username := ''
	password := ''
	subject := 'Hello from V'
	from := 'dev@vlang.io'
	to := 'dev@vlang.io'
	msg := '<h1>Hi,from V module, this message was sent by SMTP!</h1>'
	body_type := 'html'
	debug := true //use while debugging

	smtp.send_mail(server, port, username, password, subject, from, to, msg, body_type,
		debug)
}
