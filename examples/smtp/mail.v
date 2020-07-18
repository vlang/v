/*
*
* Creator: nedimf (07/2020)
*/
import os
import net.smtp

fn main() {
	println('Hi, this is sample of how to send email trough net.smtp libaray in V.')
	println('This should be really easy.\nWe  are going to create simple email client that takes some arguments. In this test we are going to send email as HTML body.')
	println('To fully test email sending I suggest using mailtrap service which is free and it actually acts like really nice sandbox.')
	println('\nEmail client V\n')
	mailserver := os.input('Enter your mailserver: ')
	from := os.input('From: ')
	println('Login')
	username := os.input('Enter your username: ')
	password := os.input('Enter your password: ')
	to := os.input('To: ')
	subject := os.input('Subject: ')
	body := os.input('Body: ')
	client_cfg := smtp.Client{
		server: mailserver
		from: from
		port: 2525
		username: username
		password: password
	}
	send_cfg := smtp.Mail{
		to: to
		subject: subject
		body_type: .html
		body: body
	}
	mut client := smtp.new_client(client_cfg) or {
		panic('Error configuring smtp')
	}
	client.send(send_cfg) or {
		panic('Error resolving email address')
	}
}
