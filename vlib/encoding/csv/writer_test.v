import encoding.csv

fn test_encoding_csv_writer_leading_whitespace() {
	for whitespace in [` `, `\t`, `\v`, `\f`, `\u0085`, `\u00a0`, `\u1680`, `\u2000`, `\u2009`,
		`\u2028`, `\u2029`, `\u202f`, `\u205f`, `\u3000`] {
		for field in [whitespace.str(), whitespace.str() + 'value'] {
			mut csv_writer := csv.new_writer()
			csv_writer.write([field, 'b'])!
			data := csv_writer.str()
			assert data == '"${field}",b\n'
			mut csv_reader := csv.new_reader(data)
			assert csv_reader.read()! == [field, 'b']
		}
	}
	mut csv_writer := csv.new_writer(delimiter: `;`)
	csv_writer.write(['   ', '\tvalue', 'b'])!
	data := csv_writer.str()
	assert data == '"   ";"\tvalue";b\n'
	mut csv_reader := csv.new_reader(data, delimiter: `;`)
	assert csv_reader.read()! == ['   ', '\tvalue', 'b']
}

fn test_encoding_csv_writer_without_leading_whitespace() {
	for field in ['', 'value ', 'some value', 'value\t', 'évalue', '\u200bvalue',
		[u8(0xa0)].bytestr()] {
		mut csv_writer := csv.new_writer()
		csv_writer.write([field, 'b'])!
		assert csv_writer.str() == '${field},b\n'
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
