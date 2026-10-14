import encoding.csv

fn test_writer_embedded_line_endings() {
	for use_crlf in [false, true] {
		for field in ['a\rb', 'a\nb', 'a\r\nb', 'a\r\r\nb', 'a"\rb'] {
			mut writer := csv.new_writer(use_crlf: use_crlf)
			writer.write(['prefix', field, 'suffix'])!
			escaped := field.replace('"', '""')
			encoded := if use_crlf {
				escaped.replace('\r\n', '\n').replace('\n', '\r\n')
			} else {
				escaped
			}
			ending := if use_crlf { '\r\n' } else { '\n' }
			output := writer.str()
			assert output == 'prefix,"${encoded}",suffix${ending}'
			mut reader := csv.new_reader(output)
			expected := field.replace('\r\n', '\n')
			assert reader.read()! == ['prefix', expected, 'suffix']
		}
	}
}

fn test_encoding_csv_writer() {
	mut csv_writer := csv.new_writer()

	csv_writer.write(['name', 'email', 'phone', 'other']) or {}
	csv_writer.write(['joe', 'joe@blow.com', '0400000000', 'test']) or {}
	csv_writer.write(['sam', 'sam@likesham.com', '0433000000', 'needs, quoting']) or {}

	assert csv_writer.str() == 'name,email,phone,other\njoe,joe@blow.com,0400000000,test\nsam,sam@likesham.com,0433000000,"needs, quoting"\n'

	/*
	mut csv_writer2 := csv.new_writer(delimiter:':')
	csv_writer.write(['foo', 'bar', '2']) or {}
	assert csv_writer.str() == 'foo:bar:2'
	*/
}

fn test_encoding_csv_writer_delimiter() {
	mut csv_writer := csv.new_writer(delimiter: ` `)

	csv_writer.write(['name', 'email', 'phone', 'other']) or {}
	csv_writer.write(['joe', 'joe@blow.com', '0400000000', 'test']) or {}
	csv_writer.write(['sam', 'sam@likesham.com', '0433000000', 'needs, quoting']) or {}

	assert csv_writer.str() == 'name email phone other\njoe joe@blow.com 0400000000 test\nsam sam@likesham.com 0433000000 "needs, quoting"\n'
}

fn test_encoding_csv_writer_preserves_carriage_returns() {
	for field in ['a\rb', '\rvalue', 'value\r', '\r', 'a"\rb'] {
		for use_crlf in [false, true] {
			mut csv_writer := csv.new_writer(use_crlf: use_crlf)
			csv_writer.write([field, 'c'])!
			data := csv_writer.str()
			line_ending := if use_crlf { '\r\n' } else { '\n' }
			assert data == '"${field.replace('"', '""')}",c${line_ending}'
			mut csv_reader := csv.new_reader(data)
			assert csv_reader.read()! == [field, 'c']
		}
	}
}

fn test_encoding_csv_writer_field_line_endings() {
	field := 'a\nb\r\nc\rd\n'
	mut csv_writer := csv.new_writer()
	csv_writer.write([field, 'c'])!
	assert csv_writer.str() == '"a\nb\r\nc\rd\n",c\n'

	mut crlf_writer := csv.new_writer(use_crlf: true)
	crlf_writer.write([field, 'c'])!
	crlf_writer.write(['next', 'row'])!
	assert crlf_writer.str() == '"a\r\nb\r\nc\rd\r\n",c\r\nnext,row\r\n'
}
