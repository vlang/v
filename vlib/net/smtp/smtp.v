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
	c.send_mailfrom(from) or {
		return error('Sending mailfrom failed: ${err}')
	}
	c.send_mailto(config.to) or {
		return error('Sending mailto failed: ${err}')
	}
	c.send_data() or {
		return error('Sending mail data failed: ${err}')
	}
	mail := Mail{
		...config
		from: from
		boundary: rand.uuid_v4()
	}
	c.send_body(mail) or {
		return error('Sending mail body failed: ${err}')
	}
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

	$if smtp_debug? {
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
	$if smtp_debug? {
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
// display name, for the SMTP envelope (`MAIL FROM:` / `RCPT TO:`), which only
// accepts a bare mailbox (RFC 5321).
fn envelope_addr(s string) string {
	_, addr_spec := split_mailbox(s)
	return addr_spec
}

// split_mailbox splits an RFC 5322 mailbox into a (display_name, addr_spec) pair.
//
// Only an angle-addr `<addr>` that is NOT inside a quoted string is treated as
// the separator, so a quoted local-part like `"a<b"@example.com` stays a
// single addr-spec. The display name is returned without surrounding quotes.
// When the input has no angle-addr pair, display_name == '' and addr_spec is
// the whole trimmed input.
//
//   'User <a@ex.com>'            ->  'User', 'a@ex.com'
//   'a@ex.com'                   ->  '', 'a@ex.com'
//   '"a<b"@example.com'          ->  '', '"a<b"@example.com'
//   'John "The Boss" <j@ex.com>' ->  'John "The Boss"', 'j@ex.com'
fn split_mailbox(s string) (string, string) {
	trimmed := s.trim_space()
	mut in_quote := false
	mut i := 0
	for i < trimmed.len {
		c := trimmed[i]
		if c == `"` {
			in_quote = !in_quote
		} else if in_quote && c == `\\` && i + 1 < trimmed.len {
			i++
		} else if c == `<` && !in_quote {
			close_idx := trimmed.index_after('>', i + 1) or { break }
			name := trimmed[..i].trim_space()
			return strip_quotes(name), trimmed[i + 1..close_idx].trim_space()
		}
		i++
	}
	return '', trimmed
}

fn strip_quotes(s string) string {
	if s.len < 2 {
		return s
	}
	if s.starts_with('"') && s.ends_with('"') && s[s.len - 2] != `\\` {
		return s[1..s.len - 1]
	}
	return s
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
	parts, attachments := cfg.mime_parts()

	message_body_len := cfg.body.len + cfg.text.body.len + cfg.html.body.len
	attachments_len := cfg.attachments.len + cfg.text.attachments.len + cfg.html.attachments.len
	mut sb := strings.new_builder(200 + message_body_len + attachments_len * 200)

	sb.write_string('From: ${format_addr(cfg.from)}\r\n')
	sb.write_string('To: ${format_addr_list(cfg.to)}\r\n')

	cc := format_addr_list(cfg.cc)
	if cc != '' {
		sb.write_string('Cc: ${cc}\r\n')
	}

	bcc := format_addr_list(cfg.bcc)
	if bcc != '' {
		sb.write_string('Bcc: ${bcc}\r\n')
	}

	date := cfg.date.custom_format('ddd, D MMM YYYY HH:mm ZZ')
	sb.write_string('Date: ${date}\r\n')

	subject := if cfg.subject.is_ascii() { cfg.subject } else { encode_rfc2047(cfg.subject) }
	sb.write_string('Subject: ${subject}\r\n')

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

// format_addr_list formats a mailbox-list for the To/Cc/Bcc headers.
// Input is separated by ';', output uses ',' per RFC 5322 #3.6.3.
//
//   'a@ex.com;b@ex.com'          ->  '<a@ex.com>, <b@ex.com>'
//   'User <a@ex.com>; b@ex.com'  ->  '"User" <a@ex.com>, <b@ex.com>'
//   'a@ex.com;;b@ex.com'         ->  '<a@ex.com>, <b@ex.com>'
//   '"Doe, John" <d@ex.com>'     ->  '"Doe, John" <d@ex.com>'
//   ''                           ->  ''
fn format_addr_list(raw string) string {
	if raw.trim_space() == '' {
		return ''
	}
	parts := raw.split(';')
	mut result := []string{}
	for part in parts {
		formatted := format_addr(part)
		if formatted != '' {
			result << formatted
		}
	}
	return result.join(', ')
}

// format_addr formats a single mailbox per RFC 5322 #3.4.
//
//   'User <u@ex.com>'  ->  '"User" <u@ex.com>'
//   'u@ex.com'         ->  '<u@ex.com>'
//   ''                 ->  ''
fn format_addr(addr string) string {
	trimmed := addr.trim_space()
	if trimmed == '' {
		return ''
	}

	display_name, addr_spec := split_mailbox(trimmed)
	if display_name == '' {
		return '<${addr_spec}>'
	}

	if !display_name.is_ascii() {
		return '${encode_rfc2047(display_name)} <${addr_spec}>'
	}

	escaped := display_name.replace('\\', '\\\\').replace('"', '\\"')
	return '"${escaped}" <${addr_spec}>'
}

// encode_rfc2047 encodes s as an RFC 2047 encoded-word ('=?utf-8?B?<base64>?=')
// for use in a message header.
//
// Note: folding an over-long value into several encoded-words is not
// implemented yet, so such a value may exceed the 75-character limit.
fn encode_rfc2047(s string) string {
	return '=?utf-8?B?${base64.encode_str(s)}?='
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
				body: cfg.text.body
			}
		}
		attachments << cfg.text.attachments
		if cfg.html.body != '' {
			parts << MimePart{
				body_type: .html
				body: cfg.html.body
			}
		}
		attachments << cfg.html.attachments
		return parts, attachments
	}
	return [MimePart{
		body_type: cfg.body_type
		body: cfg.body
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
