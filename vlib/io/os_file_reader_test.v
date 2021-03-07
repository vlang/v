import os
import io

fn read_file(file string, cap int) []string {
	mut lines := []string{}
	mut f := os.open(file) or { panic(err) }
	defer {
		f.close()
	}
	mut r := io.new_buffered_reader(reader: io.make_reader(f), cap: cap)
	for {
		l := r.read_line() or { break }
		lines << l
		// println('Line: $l')
	}
	assert lines.len > 0
	assert r.end_of_stream == true
	println('------------------------------------------------ cap: ${cap:6}; read: ${lines.len:3} lines')
	return lines
}

fn test_file_reader() {
	for cap := 1; cap <= 10000; cap += 256 {
		lines := read_file(@FILE, cap)
		assert lines.last() == '// my last line'
	}
}

// my last line
