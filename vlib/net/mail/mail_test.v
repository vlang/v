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
	parse('') or {
		assert err.msg() == 'mail: empty message'
		return
	}
	assert false, 'empty input must error'
	parse('no headers at all') or {
		assert err.msg() == 'mail: missing header/body separator'
		return
	}
	assert false, 'headerless input must error'
	bad_ct := 'From: a@example.com\nContent-Type: multipart/mixed\n\n--x\ntext\n--x--\n'
	parse(bad_ct) or {
		assert err.msg() == 'mail: multipart without boundary'
		return
	}
	assert false, 'multipart without boundary must error'
}
