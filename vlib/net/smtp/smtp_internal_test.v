module smtp

import encoding.base64

fn test_mail_message_data_with_attachment_has_valid_multipart_boundaries() {
	mail := Mail{
		from:        'sender@example.com'
		to:          'receiver@example.com'
		subject:     'Multipart test'
		body:        'message body'
		boundary:    'test-boundary'
		attachments: [
			Attachment{
				filename: 'note.txt'
				bytes:    'attachment'.bytes()
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
		from:     'sender@example.com'
		to:       'receiver@example.com'
		subject:  'Multipart alternative test'
		body:     'legacy body'
		boundary: 'test-boundary'
		text:     Message{
			body: 'text body'
		}
		html:     Message{
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
		from:     'sender@example.com'
		to:       'receiver@example.com'
		subject:  'Multipart mixed test'
		boundary: 'test-boundary'
		text:     Message{
			body: 'text body'
		}
		html:     Message{
			body:        '<p>Hello</p>'
			attachments: [
				Attachment{
					filename: 'note.txt'
					bytes:    'attachment'.bytes()
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
