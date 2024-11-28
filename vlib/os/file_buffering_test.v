import os

const tfolder = os.join_path(os.vtmp_dir(), 'os_file_buffering_tests')

fn testsuite_begin() {
	os.rmdir_all(tfolder) or {}
	assert !os.is_dir(tfolder)
	os.mkdir_all(tfolder)!
	os.chdir(tfolder)!
	assert os.is_dir(tfolder)
}

fn testsuite_end() {
	os.rmdir_all(tfolder) or {}
}

fn test_set_buffer_line_buffered() {
	dump(@LOCATION)
	mut buf := []u8{len: 25}
	dump(buf)
	mut wfile := os.open_file('text.txt', 'wb', 0o666)!
	wfile.set_buffer(mut buf, .line_buffered)
	wfile.write_string('----------------------------------\n')!
	for line in ['hello\n', 'world\n', 'hi\n'] {
		wfile.write_string(line)!
		wfile.flush()
		dump(buf)
		print(buf.bytestr())
		// assert buf.bytestr().contains(line) // this works on GLIBC, but fails on MUSL.
		unsafe { buf.reset() }
	}
	wfile.close()

	content := os.read_lines('text.txt')!
	dump(content)
	assert content == ['----------------------------------', 'hello', 'world', 'hi']
}

fn test_set_buffer_fully_buffered() {
	dump(@LOCATION)
	mut buf := []u8{len: 30}
	dump(buf)
	mut wfile := os.open_file('text.txt', 'wb', 0o666)!
	wfile.set_buffer(mut buf, .fully_buffered)
	// Ubuntu GLIBC 2.31 seems to not use the buffer for the first write call, but it does write to the buffer first for the subsequent ones.
	// MUSL (detecting the MUSL version is deliberately made hard by its authors, because of course it is :-( ...), will skip the first 8 bytes
	// of the buffer, and write everything after those.
	wfile.write_string('S')!
	wfile.write_string('---\n')!
	dump(buf)
	for line in ['hello\n', 'world\n', 'hi\n'] {
		wfile.write_string(line)!
		dump(buf)
		// print(buf.bytestr())
	}
	wfile.close()
	dump(buf)
	// assert buf.bytestr().starts_with('---\nhello\nworld\nhi\n') // works on GLIBC, fails on MUSL
	assert buf.bytestr().contains('---\nhello\nworld\n')

	content := os.read_lines('text.txt')!
	dump(content)
	assert content == ['S---', 'hello', 'world', 'hi']
}

fn test_set_unbuffered() {
	dump(@LOCATION)
	mut buf := []u8{len: 30}
	dump(buf)
	mut wfile := os.open_file('text.txt', 'wb', 0o666)!
	wfile.set_buffer(mut buf, .not_buffered)
	wfile.write_string('S')!
	wfile.write_string('---\n')!
	dump(buf)
	for line in ['hello\n', 'world\n', 'hi\n'] {
		wfile.write_string(line)!
		dump(buf)
		// print(buf.bytestr())
	}
	wfile.close()
	// dump(buf.bytestr())
	assert buf.all(it == 0)

	content := os.read_lines('text.txt')!
	dump(content)
	assert content == ['S---', 'hello', 'world', 'hi']
}
