// vtest build: present_openssl?
// Creator: nedimf (07/2020)
import os
import net.smtp

fn main() {
	println('Hi, this is sample of how to send email trough net.smtp library in V, which is really easy using the net.smtp module.')
	println('We are going to create a simple email client, that takes some arguments. and then sends email with an HTML body.')
	println('To fully test email sending, I suggest using the mailtrap.io service, which is free and acts like a really nice mail server sandbox.')
	println('')
	println('V Email client')
	println('')
	mailserver := os.input('Mail server: ')
	mailport := os.input('Mail server port: ').int()
	tls_mode := os.input('TLS mode (implicit/starttls): ').to_lower()
	ca_bundle := os.input('PEM CA bundle path: ')
	println('Login')
	username := os.input('Username: ')
	password := os.input('Password: ')
	from := os.input('From: ')
	to := os.input('To: ')
	subject := os.input('Subject: ')
	body := os.input('Body: ')
	client_cfg := smtp.Config{
		server:   mailserver
		from:     from
		port:     mailport
		username: username
		password: password
		ssl:      tls_mode == 'implicit'
		starttls: tls_mode == 'starttls'
		verify:   ca_bundle
	}
	send_cfg := smtp.Mail{
		to:        to
		subject:   subject
		body_type: .html
		body:      body
	}
	mut client := smtp.new_client(client_cfg) or { panic('Error with configuring smtp: ${err}') }
	client.send(send_cfg) or { panic('Error resolving email address: ${err}') }
}
