module smtp

/*
*
* smtp module
* Created by: nedimf (07/2020)
*/
import net
import net.ssl
import encoding.base64
import strings
import time
import io
import rand

const recv_size = 128

enum ReplyCode {
	ready      = 220
	close      = 221
	auth_ok    = 235
	action_ok  = 250
	mail_start = 354
}

pub enum BodyType {
	text
	html
}

pub struct Client {
mut:
	conn     net.TcpConn
	ssl_conn &ssl.SSLConn = unsafe { nil }
	reader   ?&io.BufferedReader
pub:
	server   string
	port     int = 25
	username string
	password string
	from     string
	ssl      bool
	starttls bool
pub mut:
	is_open   bool
	encrypted bool
}

pub struct Mail {
pub:
	from        string
	to          string
	cc          string
	bcc         string
	date        time.Time = time.now()
	subject     string
	body_type   BodyType
	body        string
	attachments []Attachment
	boundary    string
}

pub struct Attachment {
	filename string
	bytes    []u8
	cid      string
}

// new_client returns a new SMTP client and connects to it
pub fn new_client(config Client) !&Client {
	if config.ssl && config.starttls {
		return error('Can not use both implicit SSL and STARTTLS')
	}

	mut c := &Client{
		...config
	}
	c.reconnect()!
	return c
}

// reconnect reconnects to the SMTP server if the connection was closed
pub fn (mut c Client) reconnect() ! {
	if c.is_open {
		return error('Already connected to server')
	}

	conn := net.dial_tcp('${c.server}:${c.port}') or { return error('Connecting to server failed') }
	c.conn = conn

	if c.ssl {
		c.connect_ssl()!
	} else {
		c.reader = io.new_buffered_reader(reader: c.conn)
	}

	c.expect_reply(.ready) or { return error('Received invalid response from server') }
	c.send_ehlo() or { return error('Sending EHLO packet failed') }

	if c.starttls && !c.encrypted {
		c.send_starttls() or { return error('Sending STARTTLS failed') }
	}

	c.send_auth() or { return error('Authenticating to server failed') }
	c.is_open = true
}

// send sends an email
pub fn (mut c Client) send(config Mail) ! {
	if !c.is_open {
		return error('Disconnected from server')
	}
	from := if config.from != '' { config.from } else { c.from }
	c.send_mailfrom(from) or { return error('Sending mailfrom failed') }
	c.send_mailto(config.to) or { return error('Sending mailto failed') }
	c.send_data() or { return error('Sending mail data failed') }
	c.send_body(Mail{
		...config
		from:     from
		boundary: rand.uuid_v4()
	}) or { return error('Sending mail body failed') }
}

// quit closes the connection to the server
pub fn (mut c Client) quit() ! {
	c.send_str('QUIT\r\n')!
	c.expect_reply(.close)!
	if c.encrypted {
		c.ssl_conn.shutdown()!
	} else {
		c.conn.close()!
	}
	c.is_open = false
	c.encrypted = false
}

fn (mut c Client) connect_ssl() ! {
	c.ssl_conn = ssl.new_ssl_conn()!
	c.ssl_conn.connect(mut c.conn, c.server) or {
		return error('Connecting to server using OpenSSL failed: ${err}')
	}

	c.reader = io.new_buffered_reader(reader: c.ssl_conn)
	c.encrypted = true
}

// expect_reply checks if the SMTP server replied with the expected reply code
fn (mut c Client) expect_reply(expected ReplyCode) ! {
	mut str := ''
	for {
		str = c.reader or { return error('the Client.reader field is not set') }.read_line()!
		if str.len < 4 {
			return error('Invalid SMTP response: ${str}')
		}

		if str.runes()[3] == `-` {
			continue
		} else {
			break
		}
	}

	$if smtp_debug ? {
		eprintln('\n\n[RECV]')
		eprint(str)
	}

	if str.len >= 3 {
		status := str[..3].int()
		if unsafe { ReplyCode(status) } != expected {
			return error('Received unexpected status code ${status}, expecting ${expected}')
		}
	} else {
		return error('Received unexpected SMTP data: ${str}')
	}
}

@[inline]
fn (mut c Client) send_str(s string) ! {
	$if smtp_debug ? {
		eprintln('\n\n[SEND START]')
		eprint(s.trim_space())
		eprintln('\n[SEND END]')
	}

	if c.encrypted {
		c.ssl_conn.write(s.bytes())!
	} else {
		c.conn.write(s.bytes())!
	}
}

@[inline]
fn (mut c Client) send_ehlo() ! {
	c.send_str('EHLO ${c.server}\r\n')!
	c.expect_reply(.action_ok)!
}

@[inline]
fn (mut c Client) send_starttls() ! {
	c.send_str('STARTTLS\r\n')!
	c.expect_reply(.ready)!
	c.connect_ssl()!
}

@[inline]
fn (mut c Client) send_auth() ! {
	if c.username.len == 0 {
		return
	}
	mut sb := strings.new_builder(100)
	sb.write_u8(0)
	sb.write_string(c.username)
	sb.write_u8(0)
	sb.write_string(c.password)
	a := sb.str()
	auth := 'AUTH PLAIN ${base64.encode_str(a)}\r\n'
	c.send_str(auth)!
	c.expect_reply(.auth_ok)!
}

fn (mut c Client) send_mailfrom(from string) ! {
	c.send_str('MAIL FROM: <${from}>\r\n')!
	c.expect_reply(.action_ok)!
}

fn (mut c Client) send_mailto(to string) ! {
	for rcpt in to.split(';') {
		c.send_str('RCPT TO: <${rcpt}>\r\n')!
		c.expect_reply(.action_ok)!
	}
}

fn (mut c Client) send_data() ! {
	c.send_str('DATA\r\n')!
	c.expect_reply(.mail_start)!
}

fn (mut c Client) send_body(cfg Mail) ! {
	is_html := cfg.body_type == .html
	date := cfg.date.custom_format('ddd, D MMM YYYY HH:mm ZZ')
	nonascii_subject := cfg.subject.bytes().any(it < u8(` `) || it > u8(`~`))
	mut sb := strings.new_builder(200)
	sb.write_string('From: ${cfg.from}\r\n')
	sb.write_string('To: <${cfg.to.split(';').join('>; <')}>\r\n')
	sb.write_string('Cc: <${cfg.cc.split(';').join('>; <')}>\r\n')
	sb.write_string('Bcc: <${cfg.bcc.split(';').join('>; <')}>\r\n')
	sb.write_string('Date: ${date}\r\n')
	if nonascii_subject {
		// handle UTF-8 subjects according RFC 1342
		sb.write_string('Subject: =?utf-8?B?' + base64.encode_str(cfg.subject) + '?=\r\n')
	} else {
		sb.write_string('Subject: ${cfg.subject}\r\n')
	}
	if cfg.attachments.len > 0 {
		sb.write_string('MIME-Version: 1.0\r\n')
		sb.write_string('Content-Type: multipart/mixed; boundary="${cfg.boundary}"\r\n--${cfg.boundary}\r\n')
	}
	if is_html {
		sb.write_string('Content-Type: text/html; charset=UTF-8\r\n')
	} else {
		sb.write_string('Content-Type: text/plain; charset=UTF-8\r\n')
	}
	sb.write_string('Content-Transfer-Encoding: base64\r\n\r\n')
	sb.write_string(base64.encode_str(cfg.body))
	sb.write_string('\r\n')
	if cfg.attachments.len > 0 {
		sb.write_string('\r\n--${cfg.boundary}\r\n')
		sb.write_string(cfg.attachments_to_string())
	}
	sb.write_string('\r\n.\r\n')
	c.send_str(sb.str())!
	c.expect_reply(.action_ok)!
}

fn (a &Attachment) to_string(boundary string) string {
	crlf := '\r\n'
	cid := if a.cid != '' {
		'Content-ID: <${a.cid}>${crlf}'
	} else {
		''
	}
	return 'Content-Type: application/octet-stream${crlf}${cid}Content-Transfer-Encoding: base64${crlf}Content-Disposition: attachment; filename="${a.filename}"${crlf}${crlf}${base64.encode(a.bytes)}${crlf}--${boundary}${crlf}'
}

fn (m &Mail) attachments_to_string() string {
	mut res := ''
	for a in m.attachments {
		res += a.to_string(m.boundary)
	}
	return if res.len > 0 { res[..res.len - 2] } else { '' } + '\r\n'
}
