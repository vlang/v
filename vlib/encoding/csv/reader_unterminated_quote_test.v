import encoding.csv

fn test_reader_reports_unterminated_quoted_fields() {
	for data in ['"unterminated\n', 'a,"unterminated\n', '"first\nsecond', '"first\n\n',
		'"first\n#comment\n', '"escaped""\n'] {
		mut reader := csv.new_reader(data)
		reader.read() or {
			assert err.msg() == 'encoding.csv: unterminated quoted field'
			continue
		}
		assert false, 'accepted an unterminated quoted field in ${data}'
	}
}

fn test_reader_distinguishes_unterminated_field_from_clean_eof() {
	mut reader := csv.new_reader('a,b\n"unterminated\n')
	assert reader.read()! == ['a', 'b']
	reader.read() or {
		assert err.msg() == 'encoding.csv: unterminated quoted field'
		return
	}
	assert false, 'accepted an unterminated final record'
}

fn test_reader_completed_multiline_field_has_clean_eof() {
	mut reader := csv.new_reader('"first\nsecond"\n')
	assert reader.read()! == ['first\nsecond']
	reader.read() or {
		assert err.msg() == 'encoding.csv: end of file'
		return
	}
	assert false, 'read an extra record after a completed field'
}
