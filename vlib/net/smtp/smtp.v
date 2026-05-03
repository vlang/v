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

// Message stores one body variant and optional attachments for a Mail.
pub struct Message {
pub:
	body        string
	attachments []Attachment
}

// Config stores the settings used to connect a new SMTP client.
pub struct Config {
pub:
	server   string
	port     int = 25
	username string
	password string
	from     string
	ssl      bool
	starttls bool
	timeout  time.Duration
}

pub struct Client {
	Config
mut:
	conn     net.TcpConn
	ssl_conn &ssl.SSLConn = unsafe { nil }
	reader   ?&io.BufferedReader
pub mut:
	is_open   bool
	encrypted bool
}

// Mail stores the message headers and MIME payload sent by Client.send.
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
	html        Message
	text        Message
	boundary    string
}

pub struct Attachment {
pub:
	cid      string
	filename string
	bytes    []u8
}

// new_client returns a new SMTP client and connects to it
pub fn new_client(config Config) !&Client {
	if config.ssl && config.starttls {
		return error('Can not use both implicit SSL and STARTTLS')
	}

	mut c := &Client{
		Config: config
	}
	c.reconnect()!
	return c
}

// reconnect reconnects to the SMTP server if the connection was closed
pub fn (mut c Client) reconnect() ! {
	if c.is_open {
		return error('Already connected to server')
	}

	mut conn := net.dial_tcp('${c.server}:${c.port}') or {
		return error('Connecting to server failed')
	}
	if c.timeout != 0 {
		conn.set_read_timeout(c.timeout)
		conn.set_write_timeout(c.timeout)
	}
	c.conn = conn

	if c.ssl || c.encrypted {
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

// envelope_addr extracts the bare mailbox from an address that may include a
// display name. The SMTP envelope (`MAIL FROM:` / `RCPT TO:`) only accepts a
// bare mailbox (RFC 5321), while `Mail.from`/`Mail.to` may also be written in
// the RFC 5322 `Display Name <addr@example.com>` form for the message header.
fn envelope_addr(s string) string {
	trimmed := s.trim_space()
	lt := trimmed.index_u8(`<`)
	if lt < 0 {
		return trimmed
	}
	rest := trimmed[lt + 1..]
	gt := rest.index_u8(`>`)
	if gt < 0 {
		return rest
	}
	return rest[..gt]
}

fn (mut c Client) send_mailfrom(from string) ! {
	c.send_str('MAIL FROM:<${envelope_addr(from)}>\r\n')!
	c.expect_reply(.action_ok)!
}

fn (mut c Client) send_mailto(to string) ! {
	for rcpt in to.split(';') {
		c.send_str('RCPT TO:<${envelope_addr(rcpt)}>\r\n')!
		c.expect_reply(.action_ok)!
	}
}

fn (mut c Client) send_data() ! {
	c.send_str('DATA\r\n')!
	c.expect_reply(.mail_start)!
}

fn (mut c Client) send_body(cfg Mail) ! {
	c.send_str(cfg.message_data())!
	c.expect_reply(.action_ok)!
}

fn (cfg &Mail) message_data() string {
	date := cfg.date.custom_format('ddd, D MMM YYYY HH:mm ZZ')
	nonascii_subject := cfg.subject.bytes().any(it < u8(` `) || it > u8(`~`))
	parts, attachments := cfg.mime_parts()
	mut sb := strings.new_builder(200 + cfg.body.len + cfg.text.body.len + cfg.html.body.len +
		(cfg.attachments.len + cfg.text.attachments.len + cfg.html.attachments.len) * 200)
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
	if parts.len > 1 || attachments.len > 0 {
		sb.write_string('MIME-Version: 1.0\r\n')
	}

	boundary := cfg.mime_boundary()
	if parts.len > 1 && attachments.len > 0 {
		alternative_boundary := '${boundary}-alternative'
		write_multipart_header(mut sb, 'multipart/mixed', boundary)
		write_multipart_boundary(mut sb, boundary)
		write_multipart_header(mut sb, 'multipart/alternative', alternative_boundary)
		for part in parts {
			write_multipart_boundary(mut sb, alternative_boundary)
			write_message_part(mut sb, part)
		}
		write_multipart_end(mut sb, alternative_boundary)
		write_attachments(mut sb, attachments, boundary)
	} else if parts.len > 1 {
		write_multipart_header(mut sb, 'multipart/alternative', boundary)
		for part in parts {
			write_multipart_boundary(mut sb, boundary)
			write_message_part(mut sb, part)
		}
		write_multipart_end(mut sb, boundary)
	} else if attachments.len > 0 {
		write_multipart_header(mut sb, 'multipart/mixed', boundary)
		write_multipart_boundary(mut sb, boundary)
		write_message_part(mut sb, parts[0])
		write_attachments(mut sb, attachments, boundary)
	} else {
		write_message_part(mut sb, parts[0])
	}
	sb.write_string('.\r\n')
	return sb.str()
}

struct MimePart {
	body_type BodyType
	body      string
}

fn (cfg &Mail) mime_parts() ([]MimePart, []Attachment) {
	if cfg.text.body != '' || cfg.html.body != '' {
		mut parts := []MimePart{cap: 2}
		mut attachments := []Attachment{cap: cfg.text.attachments.len + cfg.html.attachments.len}
		if cfg.text.body != '' {
			parts << MimePart{
				body_type: .text
				body:      cfg.text.body
			}
		}
		attachments << cfg.text.attachments
		if cfg.html.body != '' {
			parts << MimePart{
				body_type: .html
				body:      cfg.html.body
			}
		}
		attachments << cfg.html.attachments
		return parts, attachments
	}
	return [MimePart{
		body_type: cfg.body_type
		body:      cfg.body
	}], cfg.attachments
}

fn (cfg &Mail) mime_boundary() string {
	if cfg.boundary != '' {
		return cfg.boundary
	}
	return 'v-smtp-boundary'
}

fn write_multipart_header(mut sb strings.Builder, multipart_type string, boundary string) {
	sb.write_string('Content-Type: ${multipart_type}; boundary="${boundary}"\r\n\r\n')
}

fn write_multipart_boundary(mut sb strings.Builder, boundary string) {
	sb.write_string('--${boundary}\r\n')
}

fn write_multipart_end(mut sb strings.Builder, boundary string) {
	sb.write_string('--${boundary}--\r\n')
}

fn write_message_part(mut sb strings.Builder, part MimePart) {
	if part.body_type == .html {
		sb.write_string('Content-Type: text/html; charset=UTF-8\r\n')
	} else {
		sb.write_string('Content-Type: text/plain; charset=UTF-8\r\n')
	}
	sb.write_string('Content-Transfer-Encoding: base64\r\n\r\n')
	sb.write_string(fold_base64(base64.encode_str(part.body)))
	sb.write_string('\r\n')
}

fn write_attachments(mut sb strings.Builder, attachments []Attachment, boundary string) {
	for attachment in attachments {
		write_multipart_boundary(mut sb, boundary)
		sb.write_string(attachment.to_string())
		sb.write_string('\r\n')
	}
	write_multipart_end(mut sb, boundary)
}

fn (a &Attachment) to_string() string {
	crlf := '\r\n'
	cid := if a.cid != '' {
		'Content-ID: <${a.cid}>${crlf}'
	} else {
		''
	}
	return 'Content-Type: application/octet-stream${crlf}${cid}Content-Transfer-Encoding: base64${crlf}Content-Disposition: attachment; filename="${a.filename}"${crlf}${crlf}${fold_base64(base64.encode(a.bytes))}'
}

fn fold_base64(encoded string) string {
	if encoded.len <= 76 {
		return encoded
	}
	mut sb := strings.new_builder(encoded.len + encoded.len / 76 * 2)
	for start := 0; start < encoded.len; start += 76 {
		end := if start + 76 < encoded.len { start + 76 } else { encoded.len }
		sb.write_string(encoded[start..end])
		if end < encoded.len {
			sb.write_string('\r\n')
		}
	}
	return sb.str()
}
