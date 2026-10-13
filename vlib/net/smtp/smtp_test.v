// vtest build: !windows
import os
import net.smtp
import time

// Used to test that a function call returns an error
fn fn_errors(mut c smtp.Client, m smtp.Mail) bool {
	c.send(m) or { return true }
	return false
}

fn send_mail(starttls bool) {
	ca_bundle := os.getenv('VSMTP_TEST_CA')
	client_cfg := smtp.Config{
		server:   'smtp.mailtrap.io'
		port:     if starttls { 587 } else { 465 }
		from:     'dev@vlang.io'
		username: os.getenv('VSMTP_TEST_USER')
		password: os.getenv('VSMTP_TEST_PASS')
		ssl:      !starttls
		starttls: starttls
		verify:   ca_bundle
	}
	if client_cfg.username == '' && client_cfg.password == '' {
		eprintln('Please set VSMTP_TEST_USER and VSMTP_TEST_PASS before running this test')
		exit(0)
	}
	send_cfg := smtp.Mail{
		to:      'dev@vlang.io'
		subject: 'Hello from V2'
		body:    'Plain text'
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
		cc:  'alexander@vlang.io;joe@vlang.io'
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
	if os.getenv('VSMTP_TEST_CA') == '' {
		eprintln('Please set VSMTP_TEST_CA to a PEM CA bundle before running this test')
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
	ca_bundle := os.getenv('VSMTP_TEST_CA')
	if ca_bundle == '' {
		eprintln('Please set VSMTP_TEST_CA to a PEM CA bundle before running this test')
		return
	}

	client_cfg := smtp.Config{
		server:   'smtp.gmail.com'
		port:     465
		from:     ''
		username: ''
		password: ''
		ssl:      true
		verify:   ca_bundle
	}

	mut client := smtp.new_client(client_cfg) or {
		assert false
		return
	}

	assert client.is_open && client.encrypted
}

fn test_new_client_rejects_conflicting_tls_modes() {
	client_cfg := smtp.Config{
		server:   'smtp.example.com'
		ssl:      true
		starttls: true
	}

	smtp.new_client(client_cfg) or {
		assert err.msg() == 'Can not use both implicit SSL and STARTTLS'
		return
	}

	assert false
}

fn test_tls_certificate_validation_defaults_to_on() {
	assert smtp.Config{}.validate
	assert !smtp.Config{ validate: false }.validate
}

fn test_tls_validation_requires_a_ca_bundle() {
	for config in [
		smtp.Config{ server: '127.0.0.1', port: 1, ssl: true },
		smtp.Config{ server: '127.0.0.1', port: 1, starttls: true },
	] {
		smtp.new_client(config) or {
			assert err.msg().contains('requires a CA bundle')
			continue
		}
		assert false, 'validated TLS must require an explicit trust bundle'
	}
}

fn test_plaintext_credentials_require_explicit_opt_in() {
	smtp.new_client(smtp.Config{
		server:   '127.0.0.1'
		port:     1
		username: 'user'
		password: 'password'
	}) or {
		assert err.msg().contains('refusing to send credentials without TLS')
		return
	}
	assert false, 'credentials must not be sent over plaintext by default'
}

fn test_reconnect_rejects_plaintext_auth_for_direct_clients() {
	mut client := smtp.Client{
		Config: smtp.Config{
			server:   '127.0.0.1'
			port:     1
			username: 'user'
			password: 'password'
		}
	}
	client.reconnect() or {
		assert err.msg().contains('refusing to send credentials without TLS')
		return
	}
	assert false, 'direct Client construction must not bypass the plaintext-auth guard'
}

fn test_smtp_multiple_recipients() {
	$if !network ? {
		return
	}

	assert true
}

fn test_smtp_body_base64encode() {
	$if !network ? {
		return
	}

	assert true
}
