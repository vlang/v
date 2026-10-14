module mail

import encoding.base64
import strings

// default_boundary separates MIME parts. It is only required to be unique
// within one message; encode rejects collisions with body delimiter lines.
// Override it with set_boundary when embedding messages inside each other.
const default_boundary = 'v-mail-boundary-9f3a'

// Attachment is one file carried by a Message.
pub struct Attachment {
pub:
	filename  string
	mime_type string = 'application/octet-stream'
	data      []u8
}

// Message is an email message: headers plus text/html bodies and attachments.
// Build one with the setters and encode, or read one back with parse. It is
// the message layer that `net.smtp` (send) and `net.imap`/`net.pop` (fetch)
// transport as opaque RFC 5322 text.
pub struct Message {
pub mut:
	from        string
	to          []string
	cc          []string
	subject     string
	text_body   string
	html_body   string
	attachments []Attachment
	boundary    string = default_boundary
}

// new_message creates an empty Message.
pub fn new_message() Message {
	return Message{}
}

// set_from sets the From header.
pub fn (mut m Message) set_from(addr string) {
	m.from = addr
}

// add_to appends a To recipient.
pub fn (mut m Message) add_to(addr string) {
	m.to << addr
}

// add_cc appends a Cc recipient.
pub fn (mut m Message) add_cc(addr string) {
	m.cc << addr
}

// set_subject sets the Subject header (ASCII; RFC 2047 encoding is out of
// scope and passed through verbatim).
pub fn (mut m Message) set_subject(subject string) {
	m.subject = subject
}

// set_text_body sets the plain-text body.
pub fn (mut m Message) set_text_body(body string) {
	m.text_body = body
}

// set_html_body sets the HTML body variant.
pub fn (mut m Message) set_html_body(body string) {
	m.html_body = body
}

// add_attachment attaches data under filename with a MIME type
// ('application/octet-stream' when empty).
pub fn (mut m Message) add_attachment(filename string, data []u8, mime_type string) {
	mt := if mime_type == '' { 'application/octet-stream' } else { mime_type }
	m.attachments << Attachment{filename, mt, data}
}

// set_boundary overrides the MIME part boundary.
pub fn (mut m Message) set_boundary(boundary string) {
	m.boundary = boundary
}

// encode renders the message as RFC 5322 text: plain text/plain when there is
// only a text body, multipart/alternative for text+html, and multipart/mixed
// once attachments are present.
pub fn (m &Message) encode() !string {
	mut header_values := [m.from, m.subject, m.boundary]
	header_values << m.to
	header_values << m.cc
	for value in header_values {
		validate_header_value(value)!
	}
	if !valid_boundary(m.boundary) { return error('mail: invalid MIME boundary') }
	stem := m.boundary[..if m.boundary.len < 56 { m.boundary.len } else { 56 }]
	candidate := stem + '-alternative'
	alternative_boundary := if candidate == m.boundary { 'v-mail-alternative' } else { candidate }
	mut boundaries := [m.boundary]
	if m.attachments.len > 0 && m.html_body != '' { boundaries << alternative_boundary }
	for body in [m.text_body, m.html_body] {
		for line in body.replace('\r\n', '\n').replace('\r', '\n').split('\n') {
			for boundary in boundaries {
				if line.trim_right(' \t') in ['--${boundary}', '--${boundary}--'] {
					return error('mail: MIME boundary occurs in body')
				}
			}
		}
	}
	for a in m.attachments {
		validate_header_value(a.filename)!
		validate_header_value(a.mime_type)!
	}
	mut b := strings.new_builder(1024)
	if m.from != '' {
		b.writeln('From: ${m.from}')
	}
	if m.to.len > 0 {
		b.writeln('To: ${m.to.join(', ')}')
	}
	if m.cc.len > 0 {
		b.writeln('Cc: ${m.cc.join(', ')}')
	}
	if m.subject != '' {
		b.writeln('Subject: ${m.subject}')
	}
	b.writeln('MIME-Version: 1.0')
	if m.attachments.len > 0 {
		b.writeln('Content-Type: multipart/mixed; boundary="${m.boundary}"')
		b.writeln('')
		if m.html_body != '' {
			b.writeln('--${m.boundary}')
			b.writeln('Content-Type: multipart/alternative; boundary="${alternative_boundary}"')
			b.writeln('')
			write_part(mut b, alternative_boundary, 'text/plain; charset=utf-8', '', m.text_body)
			write_part(mut b, alternative_boundary, 'text/html; charset=utf-8', '', m.html_body)
			b.writeln('--${alternative_boundary}--')
		} else {
			write_part(mut b, m.boundary, 'text/plain; charset=utf-8', '', m.text_body)
		}
		for a in m.attachments {
			write_part(mut b, m.boundary, '${a.mime_type}; name="${quote_param(a.filename)}"',
				'attachment; filename="${quote_param(a.filename)}"', wrap_base64(base64.encode(a.data)))
		}
		b.writeln('--${m.boundary}--')
	} else if m.html_body != '' {
		b.writeln('Content-Type: multipart/alternative; boundary="${m.boundary}"')
		b.writeln('')
		write_part(mut b, m.boundary, 'text/plain; charset=utf-8', '', m.text_body)
		write_part(mut b, m.boundary, 'text/html; charset=utf-8', '', m.html_body)
		b.writeln('--${m.boundary}--')
	} else {
		b.writeln('Content-Type: text/plain; charset=utf-8')
		b.writeln('Content-Transfer-Encoding: 8bit')
		b.writeln('')
		b.writeln(m.text_body)
	}
	return b.str().replace('\r\n', '\n').replace('\r', '\n').replace('\n', '\r\n')
}

fn validate_header_value(value string) ! {
	if value.contains('\r') || value.contains('\n') || value.contains('\x00') {
		return error('mail: invalid header value')
	}
}

fn quote_param(value string) string {
	return value.replace('\\', '\\\\').replace('"', '\\"')
}

// write_part writes one MIME part with an optional disposition header.
fn write_part(mut b strings.Builder, boundary string, content_type string, disposition string, body string) {
	b.writeln('--${boundary}')
	b.writeln('Content-Type: ${content_type}')
	if disposition != '' {
		b.writeln('Content-Disposition: ${disposition}')
		b.writeln('Content-Transfer-Encoding: base64')
	} else if content_type.starts_with('text/') {
		b.writeln('Content-Transfer-Encoding: 8bit')
	}
	b.writeln('')
	b.writeln(body)
}

// wrap_base64 folds base64 text to 76-column lines per RFC 2045.
fn wrap_base64(s string) string {
	if s.len <= 76 {
		return s
	}
	mut out := []string{}
	mut i := 0
	for i < s.len {
		mut end := i + 76
		if end > s.len {
			end = s.len
		}
		out << s[i..end]
		i = end
	}
	return out.join('\n')
}

// parse reads RFC 5322 text back into a Message: headers (with folding),
// text/html bodies, and base64 or 7bit attachments. Quoted-printable parts
// are an error (no decoder in vlib); header values pass through verbatim.
pub fn parse(raw string) !Message {
	if raw.trim_space() == '' {
		return error('mail: empty message')
	}
	headers, _ := split_entity(raw)!
	mut m := Message{}
	m.from = headers['from'] or { '' }
	if to := headers['to'] {
		for addr in to.split(',') {
			t := addr.trim_space()
			if t != '' {
				m.to << t
			}
		}
	}
	if cc := headers['cc'] {
		for addr in cc.split(',') {
			t := addr.trim_space()
			if t != '' {
				m.cc << t
			}
		}
	}
	m.subject = headers['subject'] or { '' }
	m.fill_from_parts([raw], 0)!
	return m
}

// parse_headers folds continuation lines and lowercases field names.
fn parse_headers(section string) map[string]string {
	mut headers := map[string]string{}
	mut last := ''
	for line in section.split('\n') {
		if line.starts_with(' ') || line.starts_with('\t') {
			if last != '' {
				headers[last] = headers[last] + ' ' + line.trim_space()
			}
			continue
		}
		colon := line.index(':') or { continue }
		last = line[..colon].trim_space().to_lower()
		headers[last] = line[colon + 1..].trim_space()
	}
	return headers
}

// content_param extracts a `key="value"` (or bare) parameter from a header.
fn content_param(header string, key string) ?string {
	mut parts := []string{}
	mut start := 0
	mut quoted := false
	mut escaped := false
	for i, ch in header {
		if escaped {
			escaped = false
		} else if quoted && ch == `\\` {
			escaped = true
		} else if ch == `"` {
			quoted = !quoted
		} else if ch == `;` && !quoted {
			parts << header[start..i]
			start = i + 1
		}
	}
	parts << header[start..]
	for part in parts[1..] {
		eq := part.index('=') or { continue }
		if part[..eq].trim_space().to_lower() != key { continue }
		value := part[eq + 1..].trim_space()
		if !value.starts_with('"') { return value }
		if value.len < 2 || !value.ends_with('"') { return none }
		mut decoded := strings.new_builder(value.len)
		mut slash := false
		for ch in value[1..value.len - 1] {
			if !slash && ch == `\\` {
				slash = true
			} else {
				decoded.write_u8(ch)
				slash = false
			}
		}
		if slash { return none }
		return decoded.str()
	}
	return none
}

// split_parts cuts a multipart body on its boundary, dropping the preamble,
// the epilogue and the closing `--`.
fn split_parts(body string, boundary string) ![]string {
	if !valid_boundary(boundary) { return error('mail: invalid MIME boundary') }
	mut parts := []string{}
	mut position := 0
	mut part_start := -1
	for position < body.len {
		line_end := if offset := body[position..].index('\n') {
			position + offset
		} else {
			body.len
		}
		line := body[position..line_end]
		without_cr := if line.ends_with('\r') { line[..line.len - 1] } else { line }
		delimiter := without_cr.trim_right(' \t')
		if delimiter == '--${boundary}' || delimiter == '--${boundary}--' {
			if part_start >= 0 {
				mut part := body[part_start..position]
				if part.ends_with('\r\n') {
					part = part[..part.len - 2]
				} else if part.ends_with('\n') {
					part = part[..part.len - 1]
				}
				parts << part
			}
			if delimiter == '--${boundary}--' {
				if part_start < 0 { return error('mail: missing opening MIME boundary') }
				return parts
			}
			part_start = line_end + 1
		}
		position = line_end + 1
	}
	return error('mail: missing closing MIME boundary')
}

fn valid_boundary(boundary string) bool {
	if boundary == '' || boundary.len > 70 || boundary.ends_with(' ') { return false }
	for ch in boundary {
		if (ch >= `a` && ch <= `z`) || (ch >= `A` && ch <= `Z`)
			|| (ch >= `0` && ch <= `9`) || ch in [`'`, `(`, `)`, `+`, `_`, `,`, `-`, `.`, `/`,
			`:`, `=`, `?`, ` `] {
			continue
		}
		return false
	}
	return true
}

fn split_entity(raw string) !(map[string]string, string) {
	if raw == '' { return map[string]string{}, '' }
	if raw.starts_with('\r\n') { return map[string]string{}, raw[2..] }
	if raw.starts_with('\n') { return map[string]string{}, raw[1..] }
	crlf_sep := raw.index('\r\n\r\n') or { -1 }
	lf_sep := raw.index('\n\n') or { -1 }
	if crlf_sep < 0 && lf_sep < 0 { return error('mail: missing header/body separator') }
	sep := if lf_sep >= 0 && (crlf_sep < 0 || lf_sep < crlf_sep) { lf_sep } else { crlf_sep }
	width := if sep == lf_sep { 2 } else { 4 }
	return parse_headers(raw[..sep].replace('\r\n', '\n')), raw[sep + width..]
}

fn decode_body(body string, encoding string) !string {
	if encoding == 'base64' {
		clean := body.replace('\r', '').replace('\n', '').replace(' ', '').replace('\t', '')
		padding := clean.len - clean.trim_right('=').len
		if clean.len % 4 != 0 || padding > 2 { return error('mail: malformed base64 body') }
		for ch in clean[..clean.len - padding] {
			if (ch >= `a` && ch <= `z`) || (ch >= `A` && ch <= `Z`)
				|| (ch >= `0` && ch <= `9`) || ch == `+` || ch == `/` {
				continue
			}
			return error('mail: malformed base64 body')
		}
		return base64.decode(clean).bytestr()
	}
	if encoding !in ['7bit', '8bit', 'binary'] {
		return error('mail: unsupported transfer encoding `${encoding}`')
	}
	return body
}

// fill_from_parts assigns text/html/attachment parts into the message.
fn (mut m Message) fill_from_parts(parts []string, depth int) ! {
	if depth > 32 { return error('mail: MIME nesting limit exceeded') }
	for part in parts {
		headers, body := split_entity(part)!
		ct := headers['content-type'] or { 'text/plain' }
		ctype := ct.all_before(';').trim_space().to_lower()
		enc := (headers['content-transfer-encoding'] or { '7bit' }).to_lower()
		disposition := headers['content-disposition'] or { '' }
		filename := content_param(disposition, 'filename') or { content_param(ct, 'name') or { '' } }
		is_attachment := disposition.all_before(';').trim_space().to_lower() == 'attachment' || filename != ''
		if ctype.starts_with('multipart/') && !is_attachment {
			if enc !in ['7bit', '8bit', 'binary'] {
				return error('mail: unsupported multipart transfer encoding `${enc}`')
			}
			boundary := content_param(ct, 'boundary') or { return error('mail: multipart without boundary') }
			m.fill_from_parts(split_parts(body, boundary)!, depth + 1)!
			continue
		}
		decoded := decode_body(body, enc)!
		if !is_attachment && ctype in ['text/plain', 'text/html'] {
			mut text := decoded.replace('\r\n', '\n').replace('\r', '\n')
			if depth == 0 && enc != 'base64' && text.ends_with('\n') { text = text[..text.len - 1] }
			if ctype == 'text/plain' && m.text_body == '' {
				m.text_body = text
			} else if ctype == 'text/html' && m.html_body == '' {
				m.html_body = text
			}
		} else {
			m.attachments << Attachment{filename, ctype, decoded.bytes()}
		}
	}
}
