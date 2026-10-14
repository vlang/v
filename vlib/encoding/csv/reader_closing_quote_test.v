import encoding.csv

fn test_reader_rejects_text_after_closing_quote() {
	for data in ['a,"b"c,d\n', '"b"c,d\n', 'a,"b"c\n', 'a,"b" ,d\n', 'a,"b\nc"x,d\n', 'a,"b""c"d,e\n'] {
		mut reader := csv.new_reader(data)
		reader.read() or {
			assert err.msg() == 'encoding.csv: unexpected character after closing quote'
			continue
		}
		assert false, 'accepted text after a closing quote in ${data}'
	}
}

fn test_reader_closing_quote_allows_delimiter_or_end_of_record() {
	mut reader := csv.new_reader('a,"b","c"\n"a""b",c,\n')
	assert reader.read()! == ['a', 'b', 'c']
	assert reader.read()! == ['a"b', 'c', '']
	mut custom := csv.new_reader('"a";"b"\n', delimiter: `;`)
	assert custom.read()! == ['a', 'b']
}
