module mail

fn test_mail_build_simple() {
	mut m := new_message()
	m.set_from('a@example.com')
	m.add_to('b@example.com')
	m.add_to('c@example.com')
	m.add_cc('d@example.com')
	m.set_subject('hi there')
	m.set_text_body('hello')
	raw := m.encode()!
	assert raw.contains('From: a@example.com')
	assert raw.contains('To: b@example.com, c@example.com')
	assert raw.contains('Cc: d@example.com')
	assert raw.contains('Subject: hi there')
	assert raw.contains('Content-Type: text/plain')
	assert raw.contains('hello')
	parsed := parse(raw)!
	assert parsed.from == 'a@example.com'
	assert parsed.to == ['b@example.com', 'c@example.com']
	assert parsed.cc == ['d@example.com']
	assert parsed.subject == 'hi there'
	assert parsed.text_body == 'hello'
	assert parsed.attachments.len == 0
}

fn test_mail_html_and_attachment_roundtrip() {
	mut m := new_message()
	m.set_from('a@example.com')
	m.add_to('b@example.com')
	m.set_subject('with files')
	m.set_text_body('see attached')
	m.set_html_body('<b>see attached</b>')
	m.add_attachment('report.pdf', 'PDFDATA'.bytes(), 'application/pdf')
	raw := m.encode()!
	parsed := parse(raw)!
	assert parsed.text_body == 'see attached'
	assert parsed.html_body == '<b>see attached</b>'
	assert parsed.attachments.len == 1
	assert parsed.attachments[0].filename == 'report.pdf'
	assert parsed.attachments[0].mime_type == 'application/pdf'
	assert parsed.attachments[0].data.bytestr() == 'PDFDATA'
}

fn test_mail_parse_fixture() {
	raw := 'From: alice@example.org\r\n' + 'To: bob@example.org\r\n' +
		'Subject: folded\r\n\tsubject line\r\n' + 'MIME-Version: 1.0\r\n' +
		'Content-Type: text/plain; charset=utf-8\r\n' + '\r\n' + 'body here\r\n'
	parsed := parse(raw)!
	assert parsed.from == 'alice@example.org'
	assert parsed.subject == 'folded subject line', 'folded headers must join'
	assert parsed.text_body == 'body here'
}

fn test_mail_parse_errors() {
	cases := {
		'':                                                                         'mail: empty message'
		'no headers at all':                                                        'mail: missing header/body separator'
		'From: a@example.com\nContent-Type: multipart/mixed\n\n--x\ntext\n--x--\n': 'mail: multipart without boundary'
	}
	for raw, expected in cases {
		if _ := parse(raw) {
			assert false, 'invalid message parsed'
		} else {
			assert err.msg() == expected
		}
	}
}

fn test_mail_first_body_part_has_opening_boundary() {
	mut m := new_message()
	m.set_text_body('plain')
	m.set_html_body('<p>html</p>')
	raw := m.encode()!
	assert raw.contains('\r\n\r\n--${m.boundary}\r\nContent-Type: text/plain')
	parsed := parse(raw)!
	assert parsed.text_body == 'plain'
	assert parsed.html_body == '<p>html</p>'
}

fn test_mail_text_attachment_and_quoted_filename_roundtrip() {
	mut m := new_message()
	m.text_body = 'body\n'
	data := [u8(0), 1, 13, 10, 255]
	m.add_attachment('notes; "quoted".txt', data, 'text/plain')
	raw := m.encode()!
	assert raw.contains('Content-Transfer-Encoding: base64')
	parsed := parse(raw)!
	assert parsed.text_body == 'body\n'
	assert parsed.attachments.len == 1
	assert parsed.attachments[0].filename == 'notes; "quoted".txt'
	assert parsed.attachments[0].data == data
}

fn test_mail_single_part_transfer_decoding_and_html_type() {
	plain := parse('Content-Type: text/plain\r\nContent-Transfer-Encoding: base64\r\n\r\naGVsbG8=\r\n')!
	assert plain.text_body == 'hello'
	html := parse('Content-Type: TEXT/HTML; charset=utf-8\nContent-Transfer-Encoding: base64\n\nPGI+aGk8L2I+\n')!
	assert html.html_body == '<b>hi</b>'
	assert html.text_body == ''
}

fn test_mail_boundaries_match_whole_lines_and_ignore_preamble_epilogue() {
	raw := 'Content-Type: Multipart/Mixed; boundary = "outer"\r\n\r\n' +
		'Content-Type: text/plain\r\n\r\npreamble\r\n--outer\r\n' +
		'Content-Type: multipart/alternative; boundary="inner"\r\n\r\n' +
		'--inner\r\nContent-Type: text/plain\r\n\r\n' +
		'body --outer inside\r\n--outer-suffix\r\n--inner--\r\n' +
		'--outer--\r\nContent-Type: text/plain\r\n\r\nepilogue'
	parsed := parse(raw)!
	assert parsed.text_body == 'body --outer inside\n--outer-suffix'
	assert parsed.attachments.len == 0
}

fn test_mail_binary_attachment_payload_is_preserved() {
	payload := 'binary\x00\r\nbytes\rbare\nend'
	raw := 'Content-Type: multipart/mixed; boundary="x"\r\n\r\n' +
		'--x\r\nContent-Type: application/octet-stream\r\n' +
		'Content-Disposition: attachment; filename="data.bin"\r\n' +
		'Content-Transfer-Encoding: binary\r\n\r\n${payload}\r\n--x--\r\n'
	parsed := parse(raw)!
	assert parsed.attachments.len == 1
	assert parsed.attachments[0].data == payload.bytes()
}

fn test_mail_encoding_rejects_header_injection_and_bad_boundaries() {
	for kind in 0 .. 8 {
		mut m := new_message()
		match kind {
			0 { m.from = 'a\r\nBcc: victim' }
			1 { m.to = ['b\nBcc: victim'] }
			2 { m.subject = 'subject\r\nInjected: yes' }
			3 { m.add_attachment('file\r\nInjected: yes', [], 'text/plain') }
			4 { m.add_attachment('file', [], 'text/plain\r\nInjected: yes') }
			5 { m.boundary = '' }
			6 { m.boundary = 'bad[boundary]' }
			else { m.text_body = 'body\n--${m.boundary}\ncollision' }
		}
		if _ := m.encode() {
			assert false, 'invalid message ${kind} encoded'
		}
	}
}

fn test_mail_transfer_and_boundary_errors() {
	for raw in [
		'Content-Type: text/plain\nContent-Transfer-Encoding: quoted-printable\n\nx=20y',
		'Content-Type: text/plain\nContent-Transfer-Encoding: base64\n\n%%%%',
		'Content-Type: multipart/mixed; boundary="x"\n\n--x\nContent-Type: text/plain\n\nx',
		'Content-Type: multipart/mixed; boundary=""\n\n--\n',
	] {
		if _ := parse(raw) {
			assert false, raw
		}
	}
}

fn test_mail_mixed_body_uses_nested_alternatives() {
	mut m := new_message()
	m.text_body = 'plain'
	m.html_body = '<b>html</b>'
	m.add_attachment('file.bin', [u8(1), 2, 3], 'application/octet-stream')
	raw := m.encode()!
	assert raw.contains('--${m.boundary}\r\nContent-Type: multipart/alternative;')
	parsed := parse(raw)!
	assert parsed.text_body == 'plain'
	assert parsed.html_body == '<b>html</b>'
	assert parsed.attachments[0].data == [u8(1), 2, 3]
}

fn test_mail_lf_headers_do_not_consume_binary_body_separator() {
	payload := 'first\r\n\r\nlast'
	parsed := parse('Content-Type: application/octet-stream\nContent-Disposition: attachment; filename="x"\nContent-Transfer-Encoding: binary\n\n${payload}')!
	assert parsed.attachments[0].data == payload.bytes()
}

fn test_mail_multiline_base64_attachment_roundtrip() {
	mut m := new_message()
	data := []u8{len: 256, init: u8(index)}
	m.add_attachment('many.bin', data, '')
	parsed := parse(m.encode()!)!
	assert parsed.attachments[0].data == data
}

fn test_mail_utf8_body_declares_8bit_encoding() {
	mut m := new_message()
	m.text_body = 'Hello, 世界'
	raw := m.encode()!
	assert raw.contains('Content-Transfer-Encoding: 8bit')
	assert parse(raw)!.text_body == m.text_body
}

fn test_mail_headerless_multipart_parts_default_to_plain_text() {
	for newline in ['\r\n', '\n'] {
		raw := 'Content-Type: multipart/mixed; boundary="x"${newline}${newline}' +
			'--x${newline}${newline}hello${newline}--x--${newline}'
		parsed := parse(raw)!
		assert parsed.text_body == 'hello'
		assert parsed.attachments.len == 0
		empty := 'Content-Type: multipart/mixed; boundary="x"${newline}${newline}' +
			'--x${newline}${newline}--x--${newline}'
		assert parse(empty)!.text_body == ''
	}
}
