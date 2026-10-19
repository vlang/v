module csv

fn test_unterminated_quoted_field_is_not_end_of_file() {
	for input in ['"', '"""\n', 'a,b,"c', '"a\nb', '"a\n\n', '"a\n#comment\n'] {
		mut reader := new_reader(input)
		if record := reader.read() {
			assert false, 'accepted truncated record: ${record}'
		} else {
			assert err is UnterminatedQuotedFieldError
			assert err.msg() == 'encoding.csv: unterminated quoted field'
		}
	}
}

fn test_complete_record_before_unterminated_quoted_field() {
	mut reader := new_reader('a,b\n"c')
	assert reader.read()! == ['a', 'b']
	if record := reader.read() {
		assert false, 'accepted truncated record: ${record}'
	} else {
		assert err is UnterminatedQuotedFieldError
	}
}

fn test_quoted_final_record_and_clean_end_of_file() {
	for input in ['""', '"a"', '"a\nb"', '"a""b"'] {
		mut reader := new_reader(input)
		assert reader.read()!.len == 1
		if record := reader.read() {
			assert false, 'unexpected extra record: ${record}'
		} else {
			assert err is EndOfFileError
		}
	}
}

fn test_reader_reports_unterminated_quoted_fields() {
	for data in ['"unterminated\n', 'a,"unterminated\n', '"first\nsecond', '"first\n\n',
		'"first\n#comment\n', '"escaped""\n'] {
		mut reader := new_reader(data)
		reader.read() or {
			assert err.msg() == 'encoding.csv: unterminated quoted field'
			continue
		}
		assert false, 'accepted an unterminated quoted field in ${data}'
	}
}

fn test_reader_distinguishes_unterminated_field_from_clean_eof() {
	mut reader := new_reader('a,b\n"unterminated\n')
	assert reader.read()! == ['a', 'b']
	reader.read() or {
		assert err.msg() == 'encoding.csv: unterminated quoted field'
		return
	}
	assert false, 'accepted an unterminated final record'
}

fn test_reader_completed_multiline_field_has_clean_eof() {
	mut reader := new_reader('"first\nsecond"\n')
	assert reader.read()! == ['first\nsecond']
	reader.read() or {
		assert err.msg() == 'encoding.csv: end of file'
		return
	}
	assert false, 'read an extra record after a completed field'
}

fn test_completed_comment_like_continuation_is_quoted_data() {
	for input in ['"first\n#comment"\n', '"first\n#comment"', '"first\n#escaped""quote"\n'] {
		mut reader := new_reader(input)
		expected := if input.contains('escaped') {
			'first\n#escaped"quote'
		} else {
			'first\n#comment'
		}
		assert reader.read()! == [expected]
		if record := reader.read() {
			assert false, 'unexpected extra record: ${record}'
		} else {
			assert err is EndOfFileError
		}
	}
}
