import os
import net.smtp
import time

// Used to test that a function call returns an error
fn fn_errors(mut c smtp.Client, m smtp.Mail) bool {
	c.send(m) or { return true }
	return false
}

fn send_mail(starttls bool) {
	client_cfg := smtp.Client{
		server: 'smtp.mailtrap.io'
		port: 465
		from: 'dev@vlang.io'
		username: os.getenv('VSMTP_TEST_USER')
		password: os.getenv('VSMTP_TEST_PASS')
		starttls: starttls
	}
	if client_cfg.username == '' && client_cfg.password == '' {
		eprintln('Please set VSMTP_TEST_USER and VSMTP_TEST_PASS before running this test')
		exit(0)
	}
	send_cfg := smtp.Mail{
		to: 'dev@vlang.io'
		subject: 'Hello from V2'
		body: 'Plain text'
	}

	mut client := smtp.new_client(client_cfg) or {
		assert false
		return
	}
	assert true
	client.send(send_cfg) or {
		assert false
		return
	}
	assert true
	client.send(smtp.Mail{
		...send_cfg
		from: 'alexander@vlang.io'
	}) or {
		assert false
		return
	}
	client.send(smtp.Mail{
		...send_cfg
		cc: 'alexander@vlang.io,joe@vlang.io'
		bcc: 'spytheman@vlang.io'
	}) or {
		assert false
		return
	}
	client.send(smtp.Mail{
		...send_cfg
		date: time.now().add_days(1000)
	}) or {
		assert false
		return
	}
	assert true
	client.quit() or {
		assert false
		return
	}
	assert true
	// This call should return an error, since the connection is closed
	if !fn_errors(mut client, send_cfg) {
		assert false
		return
	}
	client.reconnect() or {
		assert false
		return
	}
	client.send(send_cfg) or {
		assert false
		return
	}
	assert true
}

/*
*
* smtp_test
* Created by: nedimf (07/2020)
*/
fn test_smtp() {
	$if !network ? {
		return
	}

	// Test sending without STARTTLS
	send_mail(false)

	// Sleep for 10 seconds to reset the Mailtrap rate limit counter
	// See: https://help.mailtrap.io/article/44-features-and-limits#rate-limit
	time.sleep(10000 * time.millisecond)

	// Test with STARTTLS
	send_mail(true)
}

fn test_smtp_implicit_ssl() {
	$if !network ? {
		return
	}

	client_cfg := smtp.Client{
		server: 'smtp.gmail.com'
		port: 465
		from: ''
		username: ''
		password: ''
		ssl: true
	}

	mut client := smtp.new_client(client_cfg) or {
		assert false
		return
	}

	assert client.is_open && client.encrypted
}
