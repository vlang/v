import encoding.csv

fn test_writer_embedded_line_endings() {
	for use_crlf in [false, true] {
		for field in ['a\rb', 'a\nb', 'a\r\nb', 'a\r\r\nb', 'a"\rb'] {
			mut writer := csv.new_writer(use_crlf: use_crlf)
			writer.write(['prefix', field, 'suffix'])!
			escaped := field.replace('"', '""')
			encoded := if use_crlf {
				escaped.replace('\r', '').replace('\n', '\r\n')
			} else {
				escaped
			}
			ending := if use_crlf { '\r\n' } else { '\n' }
			output := writer.str()
			assert output == 'prefix,"${encoded}",suffix${ending}'
			mut reader := csv.new_reader(output)
			expected := if use_crlf { field.replace('\r', '') } else { field.replace('\r\n', '\n') }
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
