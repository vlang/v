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

fn test_fold_base64_wraps_long_lines() {
	lines := fold_base64(base64.encode_str('0123456789'.repeat(8))).split('\r\n')

	assert lines.len == 2
	assert lines[0].len == 76
	assert lines[1].len == 32
}
