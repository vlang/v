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
