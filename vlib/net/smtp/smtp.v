module smtp

/*
*
* smtp module
* Created by: nedimf (07/2020)
*/
import net
import encoding.base64
import strings
import time
import io

const (
	recv_size  = 128
)

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
	conn   net.TcpConn
	reader io.BufferedReader
pub:
	server   string
	port     int = 25
	username string
	password string
	from     string
pub mut:
    is_open  bool
}

pub struct Mail {
	from      string
	to        string
	cc        string
	bcc       string
	date      time.Time = time.now()
	subject   string
	body_type BodyType
	body      string
}

// new_client returns a new SMTP client and connects to it
pub fn new_client(config Client) ?Client {
	mut c := config
	c.reconnect()?
	return c
}

// reconnect reconnects to the SMTP server if the connection was closed
pub fn (mut c Client) reconnect() ? {
	if c.is_open { return error('Already connected to server') }

	conn := net.dial_tcp('$c.server:$c.port') or { return error('Connecting to server failed') }
	c.conn = conn

	c.reader = io.new_buffered_reader(reader: io.make_reader(c.conn))

	c.expect_reply(.ready) or { return error('Received invalid response from server') }
	c.send_ehlo() or { return error('Sending EHLO packet failed') }
	c.send_auth() or { return error('Authenticating to server failed') }
	c.is_open = true
}

// send sends an email
pub fn (c Client) send(config Mail) ? {
	if !c.is_open { return error('Disconnected from server') }
	from := if config.from != '' { config.from } else { c.from }
	c.send_mailfrom(from) or { return error('Sending mailfrom failed') }
	c.send_mailto(config.to) or { return error('Sending mailto failed') }
	c.send_data() or { return error('Sending mail data failed') }
	c.send_body({ config | from: from }) or { return error('Sending mail body failed') }
}

// quit closes the connection to the server
pub fn (mut c Client) quit() ? {
	c.send_str('QUIT\r\n')
	c.expect_reply(.close)
	c.conn.close()?
	c.is_open = false
}

// expect_reply checks if the SMTP server replied with the expected reply code
fn (c Client) expect_reply(expected ReplyCode) ? {
	bytes := io.read_all(reader: c.conn)?

	str := bytes.bytestr().trim_space()
	$if smtp_debug? {
		eprintln('\n\n[RECV]')
		eprint(str)
	}

	if str.len >= 3 {
		status := str[..3].int()
		if ReplyCode(status) != expected {
			return error('Received unexpected status code $status, expecting $expected')
		}
	} else { return error('Recieved unexpected SMTP data: $str') }
}

[inline]
fn (c Client) send_str(s string) ? {
	$if smtp_debug? {
		eprintln('\n\n[SEND START]')
		eprint(s.trim_space())
		eprintln('\n[SEND END]')
	}
	c.conn.write(s.bytes())?
}

[inline]
fn (c Client) send_ehlo() ? {
	c.send_str('EHLO $c.server\r\n')?
	c.expect_reply(.action_ok)?
}

[inline]
fn (c Client) send_auth() ? {
	if c.username.len == 0 {
		return
	}    
	mut sb := strings.new_builder(100)
	sb.write_b(0)
	sb.write(c.username)
	sb.write_b(0)
	sb.write(c.password)
	a := sb.str()
	auth := 'AUTH PLAIN ${base64.encode(a)}\r\n'
	c.send_str(auth)?
	c.expect_reply(.auth_ok)?
}

fn (c Client) send_mailfrom(from string) ? {
	c.send_str('MAIL FROM: <$from>\r\n')?
	c.expect_reply(.action_ok)?
}

fn (c Client) send_mailto(to string) ? {
	c.send_str('RCPT TO: <$to>\r\n')?
	c.expect_reply(.action_ok)?
}

fn (c Client) send_data() ? {
	c.send_str('DATA\r\n')?
	c.expect_reply(.mail_start)
}

fn (c Client) send_body(cfg Mail) ? {
	is_html := cfg.body_type == .html
	date := cfg.date.utc_string().trim_right(' UTC') // TODO
	mut sb := strings.new_builder(200)
	sb.write('From: $cfg.from\r\n')
	sb.write('To: <$cfg.to>\r\n')
	sb.write('Cc: <$cfg.cc>\r\n')
	sb.write('Bcc: <$cfg.bcc>\r\n')
	sb.write('Date: $date\r\n')
	sb.write('Subject: $cfg.subject\r\n')
	if is_html {
		sb.write('Content-Type: text/html; charset=ISO-8859-1')
	}
	sb.write('\r\n\r\n')
	sb.write(cfg.body)
	sb.write('\r\n.\r\n')
	c.send_str(sb.str())?
	c.expect_reply(.action_ok)?
}
