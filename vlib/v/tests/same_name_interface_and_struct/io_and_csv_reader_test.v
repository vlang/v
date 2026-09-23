module main

import io
import encoding.csv
import memsource

// https://github.com/vlang/v/issues/28834
// `encoding.csv.Reader.read()` must not replace the signature of the
// `io.Reader.read(mut buf []u8)` interface method dispatcher.
fn test_io_buffered_reader_with_csv_reader_imported() {
	mut reader := io.new_buffered_reader(reader: memsource.new('a,b\n1,2\n'))
	mut rows := [][]string{}
	for {
		line := reader.read_line() or { break }
		mut r := csv.new_reader(line + '\n')
		rows << r.read() or { break }
	}
	assert rows == [['a', 'b'], ['1', '2']]
}
