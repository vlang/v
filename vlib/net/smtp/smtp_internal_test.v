module smtp

import encoding.base64

fn test_mail_message_data_with_attachment_has_valid_multipart_boundaries() {
	mail := Mail{
		from: 'sender@example.com'
		to: 'receiver@example.com'
		subject: 'Multipart test'
		body: 'message body'
		boundary: 'test-boundary'
		attachments: [
			Attachment{
				filename: 'note.txt'
				bytes: 'attachment'.bytes()
			},
		]
	}

	message := mail.message_data()

	assert message.contains('Content-Type: multipart/mixed; boundary="test-boundary"\r\n\r\n--test-boundary\r\n')
	assert message.contains('Content-Type: text/plain; charset=UTF-8\r\nContent-Transfer-Encoding: base64\r\n\r\nbWVzc2FnZSBib2R5\r\n--test-boundary\r\n')
	assert message.contains('Content-Disposition: attachment; filename="note.txt"\r\n\r\nYXR0YWNobWVudA==\r\n--test-boundary--\r\n.\r\n')
	assert !message.contains('Content-Type: multipart/mixed; boundary="test-boundary"\r\n--test-boundary\r\n')
	assert !message.contains('YXR0YWNobWVudA==\r\n--test-boundary\r\n.\r\n')
}

fn test_mail_message_data_with_text_and_html_uses_multipart_alternative() {
	mail := Mail{
		from: 'sender@example.com'
		to: 'receiver@example.com'
		subject: 'Multipart alternative test'
		body: 'legacy body'
		boundary: 'test-boundary'
		text: Message{
			body: 'text body'
		}
		html: Message{
			body: '<h1>Hello</h1>'
		}
	}

	message := mail.message_data()

	assert message.contains('MIME-Version: 1.0\r\nContent-Type: multipart/alternative; boundary="test-boundary"\r\n\r\n--test-boundary\r\n')
	assert message.contains('Content-Type: text/plain; charset=UTF-8\r\nContent-Transfer-Encoding: base64\r\n\r\n${base64.encode_str('text body')}\r\n--test-boundary\r\n')
	assert message.contains('Content-Type: text/html; charset=UTF-8\r\nContent-Transfer-Encoding: base64\r\n\r\n${base64.encode_str('<h1>Hello</h1>')}\r\n--test-boundary--\r\n.\r\n')
	assert !message.contains(base64.encode_str('legacy body'))
	assert !message.contains('multipart/mixed')
}

fn test_mail_message_data_with_text_html_and_attachment_uses_nested_multipart() {
	mail := Mail{
		from: 'sender@example.com'
		to: 'receiver@example.com'
		subject: 'Multipart mixed test'
		boundary: 'test-boundary'
		text: Message{
			body: 'text body'
		}
		html: Message{
			body: '<p>Hello</p>'
			attachments: [
				Attachment{
					filename: 'note.txt'
					bytes: 'attachment'.bytes()
				},
			]
		}
	}

	message := mail.message_data()

	assert message.contains('Content-Type: multipart/mixed; boundary="test-boundary"\r\n\r\n--test-boundary\r\nContent-Type: multipart/alternative; boundary="test-boundary-alternative"\r\n\r\n--test-boundary-alternative\r\n')
	assert message.contains('Content-Type: text/plain; charset=UTF-8\r\nContent-Transfer-Encoding: base64\r\n\r\n${base64.encode_str('text body')}\r\n--test-boundary-alternative\r\n')
	assert message.contains('Content-Type: text/html; charset=UTF-8\r\nContent-Transfer-Encoding: base64\r\n\r\n${base64.encode_str('<p>Hello</p>')}\r\n--test-boundary-alternative--\r\n')
	assert message.contains('--test-boundary\r\nContent-Type: application/octet-stream\r\nContent-Transfer-Encoding: base64\r\nContent-Disposition: attachment; filename="note.txt"\r\n\r\n${base64.encode_str('attachment')}\r\n--test-boundary--\r\n.\r\n')
}

fn test_fold_base64_wraps_long_lines() {
	lines := fold_base64(base64.encode_str('0123456789'.repeat(8))).split('\r\n')

	assert lines.len == 2
	assert lines[0].len == 76
	assert lines[1].len == 32
}

fn test_envelope_addr_strips_display_name() {
	assert envelope_addr('ivan@example.com') == 'ivan@example.com'
	assert envelope_addr('  ivan@example.com  ') == 'ivan@example.com'
	assert envelope_addr('<ivan@example.com>') == 'ivan@example.com'
	assert envelope_addr('Ivan Petrov <ivan@example.com>') == 'ivan@example.com'
	assert envelope_addr('"Petrov, Ivan" <ivan@example.com>') == 'ivan@example.com'
	// Quoted local-parts may legitimately contain '<'. Without a trailing '>',
	// the input is not an angle-addr wrapper and must pass through unchanged.
	assert envelope_addr('"a<b"@example.com') == '"a<b"@example.com'
	// Quoted display name and quoted local-part both containing '<'/'>'.
	assert envelope_addr('"a<b" <"a<b"@example.com>') == '"a<b"@example.com'
	// Escaped quote inside a quoted display name must not end the quoted run.
	assert envelope_addr('"a\\"<b" <ivan@example.com>') == 'ivan@example.com'
	// Angle brackets inside a quoted local-part must not be treated as the
	// envelope separator — only the outermost '<' / '>' pair counts.
	assert envelope_addr('User <"a>b"@example.com>') == '"a>b"@example.com'
	assert envelope_addr('User <"a>b@c<d"@example.com>') == '"a>b@c<d"@example.com'
	// Malformed input (no closing '>') passes through; the server can reject.
	assert envelope_addr('Ivan <ivan@example.com') == 'Ivan <ivan@example.com'
	// A bare address must still be CRLF-sanitized to keep the newline out of RCPT TO.
	assert envelope_addr('victim@example.com\nX-Evil: yes') == 'victim@example.comX-Evil: yes'
}

fn test_mail_message_data_preserves_display_name_in_from_header() {
	mail := Mail{
		from: 'Ivan Petrov <ivan@example.com>'
		to: 'recipient@example.com'
		subject: 'Test'
		body: 'hi'
	}

	message := mail.message_data()

	// the display name is preserved and quoted per RFC 5322
	assert message.contains('From: "Ivan Petrov" <ivan@example.com>\r\n')
}

fn test_mail_message_data_wraps_bare_from_addr() {
	mail := Mail{
		from: 'sender@example.com'
		to: 'receiver@example.com'
		subject: 'Test'
		body: 'hi'
	}

	message := mail.message_data()

	// a bare from address is wrapped in angle brackets
	assert message.contains('From: <sender@example.com>\r\n')
}

fn test_mail_message_data_encodes_non_ascii_from() {
	// non-ASCII display names must become an RFC 2047 encoded-word, not raw UTF-8
	mail := Mail{
		from: 'Иван Петров <ivan@example.com>'
		to: 'recipient@example.com'
		subject: 'Test'
		body: 'hi'
	}

	message := mail.message_data()

	assert message.contains('From: =?utf-8?B?0JjQstCw0L0g0J/QtdGC0YDQvtCy?= <ivan@example.com>\r\n')
}

fn test_mail_message_data_omits_empty_cc_and_bcc_headers() {
	mail := Mail{
		from: 'sender@example.com'
		to: 'receiver@example.com'
		subject: 'No cc/bcc test'
	}

	message := mail.message_data()

	// empty Cc/Bcc must not produce an empty header line
	assert !message.contains('Cc:')
	assert !message.contains('Bcc:')
}

fn test_mail_message_data_includes_cc_header_but_omits_bcc() {
	mail := Mail{
		from: 'sender@example.com'
		to: 'receiver@example.com; '
		cc: '<copy@example.com>; One <one@example.com>;'
		bcc: 'hidden@example.com;Two <two@example.com>'
		subject: 'Cc/bcc test'
	}

	message := mail.message_data()

	// ';'-separated input becomes a ','-joined Cc header.
	assert message.contains('Cc: <copy@example.com>, "One" <one@example.com>\r\n')
	// Bcc addresses are envelope-only per RFC 5321; they must not appear in DATA headers.
	assert !message.contains('Bcc:')
}

fn test_mail_message_data_formats_to_header() {
	// bare ';'-separated addresses become a ','-joined list
	mail := Mail{
		from: 'sender@example.com'
		to: 'a@ex.com;b@ex.com'
		subject: 'To test'
	}
	message := mail.message_data()
	assert message.contains('To: <a@ex.com>, <b@ex.com>\r\n')

	// a display name is quoted instead of producing nested angle brackets
	mail2 := Mail{
		from: 'sender@example.com'
		to: 'Ivan Petrov <ivan@example.com>'
		subject: 'To test'
	}
	message2 := mail2.message_data()
	assert message2.contains('To: "Ivan Petrov" <ivan@example.com>\r\n')
}

fn test_mail_message_data_encodes_non_ascii_subject() {
	// an ASCII subject stays raw
	mail := Mail{
		from: 'sender@example.com'
		to: 'receiver@example.com'
		subject: 'Hello world'
	}
	message := mail.message_data()
	assert message.contains('Subject: Hello world\r\n')

	// a non-ASCII subject becomes an RFC 2047 encoded-word, not raw UTF-8
	mail2 := Mail{
		from: 'sender@example.com'
		to: 'receiver@example.com'
		subject: 'Привет мир'
	}
	message2 := mail2.message_data()
	assert message2.contains('Subject: =?utf-8?B?0J/RgNC40LLQtdGCINC80LjRgA==?=\r\n')
}

fn test_format_addr() {
	// a bare or already angle-wrapped address keeps its addr-spec
	assert format_addr('user@ex.com') == '<user@ex.com>'
	assert format_addr('<user@ex.com>') == '<user@ex.com>'

	// a display name is quoted and angle-wrapped
	assert format_addr('User <user@ex.com>') == '"User" <user@ex.com>'
	assert format_addr('John Smith <john@ex.com>') == '"John Smith" <john@ex.com>'

	// embedded quotes and backslashes inside the display name are escaped
	assert format_addr('John "The Boss" <john@ex.com>') == '"John \\"The Boss\\"" <john@ex.com>'

	assert format_addr('C:\\Users <user@ex.com>') == '"C:\\\\Users" <user@ex.com>'

	// an even number of backslashes before the closing quote must not defeat
	// quote stripping; quoted-pairs are decoded to their underlying character
	// before re-escaping on output.
	assert format_addr('"A\\(B" <x@example.com>') == '"A(B" <x@example.com>'
	// a quoted-pair may encode a literal backslash at the end of the name
	assert format_addr('"A\\\\" <x@example.com>') == '"A\\\\" <x@example.com>'

	// non-ASCII display names become an RFC 2047 encoded-word
	assert format_addr('Иван Петров <ivan@ex.com>') == '=?utf-8?B?0JjQstCw0L0g0J/QtdGC0YDQvtCy?= <ivan@ex.com>'
	assert format_addr('"Иван Петров" <ivan@ex.com>') == '=?utf-8?B?0JjQstCw0L0g0J/QtdGC0YDQvtCy?= <ivan@ex.com>'

	// CR/LF inside a display name must be stripped to prevent header injection
	// via SMTP DATA.  Two occurrences verify the replacement covers the whole
	// string, not just the first match.
	assert format_addr('Bad\r\nX\r\nEvil <a@b.com>') == '"BadXEvil" <a@b.com>'
	// CRLF inside the addr-spec is stripped too, so it cannot leak into the
	// SMTP envelope or the header.
	assert format_addr('<a@b.com\r\nX: evil>') == '<a@b.comX: evil>'
	// A bare address must still be CRLF-sanitized to keep the newline out of the header.
	assert format_addr('victim@example.com\nX-Evil: yes') == '<victim@example.comX-Evil: yes>'
}

fn test_format_addr_list() {
	// an empty or whitespace-only list produces no header content
	assert format_addr_list('') == ''
	assert format_addr_list('   ') == ''

	// a single address is formatted just like format_addr
	assert format_addr_list('user@ex.com') == '<user@ex.com>'
	assert format_addr_list('<user@ex.com>') == '<user@ex.com>'

	// input is ';'-separated; output is ','-joined, empty entries dropped
	assert format_addr_list('a@ex.com;b@ex.com') == '<a@ex.com>, <b@ex.com>'
	assert format_addr_list('User <a@ex.com>; b@ex.com') == '"User" <a@ex.com>, <b@ex.com>'
	assert format_addr_list('a@ex.com;;b@ex.com') == '<a@ex.com>, <b@ex.com>'
	assert format_addr_list('John <a@ex.com>; Bob <b@ex.com>') == '"John" <a@ex.com>, "Bob" <b@ex.com>'

	// a comma inside a quoted display name must survive formatting
	assert format_addr_list('"Doe, John" <john.doe@ex.com>') == '"Doe, John" <john.doe@ex.com>'
	assert format_addr_list('"Doe, John" <john.doe@ex.com>; "Roe, Jane" <jane@ex.com>') == '"Doe, John" <john.doe@ex.com>, "Roe, Jane" <jane@ex.com>'

	// each non-ASCII display name is encoded independently
	assert format_addr_list('Иван Петров <ivan@ex.com>') == '=?utf-8?B?0JjQstCw0L0g0J/QtdGC0YDQvtCy?= <ivan@ex.com>'
	assert format_addr_list('Иван <ivan@ex.com>; John <a@ex.com>') == '=?utf-8?B?0JjQstCw0L0=?= <ivan@ex.com>, "John" <a@ex.com>'
}
