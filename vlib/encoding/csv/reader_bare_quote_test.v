import encoding.csv

fn test_reader_rejects_bare_quotes() {
	for data in ['a"b,c\n', 'a,b"c,d\n', 'a,b"c\n', 'a, b"c,d\n', 'a,b""c,d\n'] {
		mut reader := csv.new_reader(data)
		reader.read() or {
			assert err.msg() == 'encoding.csv: bare quote in non-quoted field'
			continue
		}
		assert false, 'accepted a bare quote in ${data}'
	}
}

fn test_reader_bare_quote_check_allows_quoted_fields() {
	mut reader := csv.new_reader('a,"b""c",d\n"a",b,"c"\n')
	assert reader.read()! == ['a', 'b"c', 'd']
	assert reader.read()! == ['a', 'b', 'c']
	mut custom := csv.new_reader('a;"b""c";d\n', delimiter: `;`)
	assert custom.read()! == ['a', 'b"c', 'd']
}

fn test_closing_quote_requires_delimiter_or_record_end() {
	for data in ['a,"b"c,d', 'a,"b" ,c', 'a,"b"c', '"b"c,d', 'a,"b""c"d,e', 'a,"b\nc"d,e'] {
		mut reader := csv.new_reader(data)
		reader.read() or {
			assert err.msg() == 'encoding.csv: bare quote in non-quoted field'
			continue
		}
		assert false, 'accepted invalid CSV: ${data}'
	}
}

fn test_custom_delimiter_does_not_hide_bare_quotes() {
	for data in ['11,"12\n13"\n21,"2""2""\n23"\n"3""1""",32\n', 'a1,"b1",c1\n"a2",b2,c2\n', 'a;"b"c;d',
		'"b" ;c'] {
		mut reader := csv.new_reader(data, delimiter: `;`)
		reader.read() or {
			assert err.msg() == 'encoding.csv: bare quote in non-quoted field'
			continue
		}
		assert false, 'accepted invalid CSV with semicolon delimiter: ${data}'
	}
}

fn test_closing_quotes_with_valid_record_boundaries() {
	mut reader := csv.new_reader('a;"b";"c""d"\n"e";"f\ng";\n', delimiter: `;`)
	assert reader.read()! == ['a', 'b', 'c"d']
	assert reader.read()! == ['e', 'f\ng', '']
}
