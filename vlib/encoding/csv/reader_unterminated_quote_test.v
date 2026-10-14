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
