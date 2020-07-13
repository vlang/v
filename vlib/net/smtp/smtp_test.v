import os
import smtp
import time

// Used to test that a function call returns an error
fn fn_errors(c smtp.Client, m smtp.Mail) bool {
	c.send(m) or { return true }
	return false
}

/*
*
* smtp_test
* Created by: nedimf (07/2020)
*/
fn test_smtp() {
	$if !network ? { return }

	client_cfg := smtp.Client{
		server: 'smtp.mailtrap.io'
		from: 'dev@vlang.io'
		username: os.getenv('VSMTP_TEST_USER')
		password: os.getenv('VSMTP_TEST_PASS')
	}

	send_cfg := smtp.Mail{
		to: 'dev@vlang.io'
		subject: 'Hello from V2'
		body: 'Plain text'
	}

	// This loop avoids `or { assert false  return }` after each call; instead, it's replaced with
	// `break`. There's an `assert false` after the loop, which will only be called if this loop
	// is broken from.
	for {
		mut client := smtp.new_client(client_cfg) or { assert false break }
		client.send(send_cfg) or { assert false break }
		// client.send({ send_cfg | body_type: .html, body: '<html><h1>HTML V email!</h1></html>' }) or { break }
		client.send({ send_cfg | from: 'alexander@vlang.io' }) or { assert false break }
		client.send({ send_cfg | cc: 'alexander@vlang.io,joe@vlang.io', bcc: 'spytheman@vlang.io' }) or { assert false break }
		client.send({ send_cfg | date: time.now().add_days(1000) }) or { assert false break }
		client.quit() or { assert false break }
		// This call should return an error, since the connection is closed
		if !fn_errors(client, send_cfg) { assert false break }
		client.reconnect() or { assert false break }
		client.send(send_cfg) or { assert false break }
		assert true
		return
	}
	assert false
}
