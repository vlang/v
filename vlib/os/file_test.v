import os

const tfolder = os.join_path(os.vtmp_dir(), 'os_file_tests')
const tfile = os.join_path_single(tfolder, 'test_file')

fn testsuite_begin() {
	os.mkdir_all(tfolder) or {}
	os.chdir(tfolder)!
	assert os.is_dir(tfolder)
}

fn testsuite_end() {
	os.rmdir_all(tfolder) or {}
}

struct Point {
	x f64
	y f64
	z f64
}

struct Extended_Point {
	a f64
	b f64
	c f64
	d f64
	e f64
	f f64
	g f64
	h f64
	i f64
}

enum Color {
	red
	green
	blue
}

@[flag]
enum Permissions {
	read
	write
	execute
}

const unit_point = Point{1.0, 1.0, 1.0}
const another_point = Point{0.25, 2.25, 6.25}
const extended_point = Extended_Point{1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0}
const another_byte = u8(123)
const another_color = Color.red
const another_permission = Permissions.read | .write

// test_read_bytes_with_newline_text tests reading text from a file with newlines.
// This test simulates reading a larger text file step by step into a buffer and
// returning on each newline, even before the buffer is full, and reaching EOF before
// the buffer is completely filled.
fn test_read_bytes_with_newline_text() {
	mut f := os.open_file(tfile, 'w')!
	f.write_string('Hello World!\nGood\r morning.')!
	f.close()

	f = os.open_file(tfile, 'r')!
	mut buf := []u8{len: 8}

	n0 := f.read_bytes_with_newline(mut buf)!
	assert n0 == 8

	n1 := f.read_bytes_with_newline(mut buf)!
	assert n1 == 5

	n2 := f.read_bytes_with_newline(mut buf)!
	assert n2 == 8

	n3 := f.read_bytes_with_newline(mut buf)!
	assert n3 == 6

	f.close()
}

// test_read_bytes_with_newline_binary tests reading a binary file with NUL bytes.
// This test simulates the scenario when a byte stream is read and a newline byte
// appears in that stream and an EOF occurs before the buffer is full.
fn test_read_bytes_with_newline_binary() {
	os.rm(tfile) or {} // FIXME: This is a workaround for macos, because the file isn't truncated when open with 'w'
	mut bw := []u8{len: 15}
	bw[9] = 0xff
	bw[12] = 10 // newline

	n0_bytes := unsafe { bw[0..10] }
	n1_bytes := unsafe { bw[10..13] }
	n2_bytes := unsafe { bw[13..] }

	mut f := os.open_file(tfile, 'w')!
	f.write(bw)!
	f.close()

	f = os.open_file(tfile, 'r')!
	mut buf := []u8{len: 10}

	n0 := f.read_bytes_with_newline(mut buf)!
	assert n0 == 10
	assert buf[..n0] == n0_bytes

	n1 := f.read_bytes_with_newline(mut buf)!
	assert n1 == 3
	assert buf[..n1] == n1_bytes

	n2 := f.read_bytes_with_newline(mut buf)!
	assert n2 == 2
	assert buf[..n2] == n2_bytes
	f.close()
}

// test_read_eof_last_read_partial_buffer_fill tests that when reading a file
// the end-of-file is detected and results in a none error being returned. This
// test simulates file reading where the end-of-file is reached inside an fread
// containing data.
fn test_read_eof_last_read_partial_buffer_fill() {
	mut f := os.open_file(tfile, 'w')!
	bw := []u8{len: 199, init: 5}
	f.write(bw)!
	f.close()

	f = os.open_file(tfile, 'r')!
	mut br := []u8{len: 100}
	// Read first 100 bytes of 199 byte file, should fill buffer with no error.
	n0 := f.read(mut br)!
	assert n0 == 100
	// Read remaining 99 bytes of 199 byte file, should fill buffer with no
	// error, even though end-of-file was reached.
	n1 := f.read(mut br)!
	assert n1 == 99
	// Read again, end-of-file was previously reached so should return none
	// error.
	if _ := f.read(mut br) {
		// This is not intended behavior because the read function should
		// not return a number of bytes read when end-of-file is reached.
		assert false
	} else {
		// Expected an error when received end-of-file.
		assert err is os.Eof
	}
	f.close()
}

// test_read_eof_last_read_full_buffer_fill tests that when reading a file the
// end-of-file is detected and results in a none error being returned. This test
// simulates file reading where the end-of-file is reached at the beginning of an
// fread that returns no data.
fn test_read_eof_last_read_full_buffer_fill() {
	mut f := os.open_file(tfile, 'w')!
	bw := []u8{len: 200, init: 5}
	f.write(bw)!
	f.close()

	f = os.open_file(tfile, 'r')!
	mut br := []u8{len: 100}
	// Read first 100 bytes of 200 byte file, should fill buffer with no error.
	n0 := f.read(mut br)!
	assert n0 == 100
	// Read remaining 100 bytes of 200 byte file, should fill buffer with no
	// error. The end-of-file isn't reached yet, but there is no more data.
	n1 := f.read(mut br)!
	assert n1 == 100
	// Read again, end-of-file was previously reached so should return none
	// error.
	if _ := f.read(mut br) {
		// This is not intended behavior because the read function should
		// not return a number of bytes read when end-of-file is reached.
		assert false
	} else {
		// Expect an error at EOF.
		assert err is os.Eof
	}
	f.close()
}

fn test_write_struct() {
	os.rm(tfile) or {} // FIXME: This is a workaround for macos, because the file isn't truncated when open with 'w'
	size_of_point := int(sizeof(Point))
	mut f := os.open_file(tfile, 'w')!
	f.write_struct(another_point)!
	f.close()
	x := os.read_file(tfile)!
	pcopy := unsafe { &u8(memdup(&another_point, size_of_point)) }
	y := unsafe { pcopy.vstring_with_len(size_of_point) }
	assert x == y
	$if debug {
		eprintln(x.bytes())
		eprintln(y.bytes())
	}
}

fn test_write_struct_at() {
	mut f := os.open_file(tfile, 'w')!
	f.write_struct(extended_point)!
	f.write_struct_at(another_point, 3)!
	f.close()
	f = os.open_file(tfile, 'r')!
	mut p := Point{}
	f.read_struct_at(mut p, 3)!
	f.close()

	assert p == another_point
}

fn test_read_struct() {
	mut f := os.open_file(tfile, 'w')!
	f.write_struct(another_point)!
	f.close()

	f = os.open_file(tfile, 'r')!
	mut p := Point{}
	f.read_struct(mut p)!
	f.close()

	assert p == another_point
}

fn test_read_struct_at() {
	mut f := os.open_file(tfile, 'w')!
	f.write([u8(1), 2, 3])!
	f.write_struct(another_point)!
	f.close()
	f = os.open_file(tfile, 'r')!
	mut p := Point{}
	f.read_struct_at(mut p, 3)!
	f.close()

	assert p == another_point
}

fn test_write_raw() {
	os.rm(tfile) or {} // FIXME: This is a workaround for macos, because the file isn't truncated when open with 'w'
	size_of_point := int(sizeof(Point))
	mut f := os.open_file(tfile, 'w')!
	f.write_raw(another_point)!
	f.close()
	x := os.read_file(tfile)!
	pcopy := unsafe { &u8(memdup(&another_point, size_of_point)) }
	y := unsafe { pcopy.vstring_with_len(size_of_point) }
	assert x == y
	$if debug {
		eprintln(x.bytes())
		eprintln(y.bytes())
	}
}

fn test_write_raw_at() {
	mut f := os.open_file(tfile, 'w')!
	f.write_raw(extended_point)!
	f.write_raw_at(another_point, 3)!
	f.close()
	f = os.open_file(tfile, 'r')!
	mut p := Point{}
	f.read_struct_at(mut p, 3)!
	f.close()

	assert p == another_point
}

fn test_write_raw_at_negative_pos() {
	mut f := os.open_file(tfile, 'w')!
	if _ := f.write_raw_at(another_point, u64(-1)) {
		assert false
	}
	f.write_raw_at(another_point, u64(-1)) or { assert err.msg() == 'Invalid argument' }
	f.close()
}

fn test_read_raw() {
	mut f := os.open_file(tfile, 'w')!
	f.write_raw(another_point)!
	f.write_raw(another_byte)!
	f.write_raw(another_color)!
	f.write_raw(another_permission)!
	f.close()
	f = os.open_file(tfile, 'r')!
	p := f.read_raw[Point]()!
	b := f.read_raw[u8]()!
	c := f.read_raw[Color]()!
	x := f.read_raw[Permissions]()!
	f.close()

	assert p == another_point
	assert b == another_byte
	assert c == another_color
	assert x == another_permission
}

fn test_read_raw_at() {
	mut f := os.open_file(tfile, 'w')!
	f.write([u8(1), 2, 3])!
	f.write_raw(another_point)!
	f.write_raw(another_byte)!
	f.write_raw(another_color)!
	f.write_raw(another_permission)!
	f.close()
	f = os.open_file(tfile, 'r')!
	mut at := u64(3)
	p := f.read_raw_at[Point](at)!
	at += sizeof(Point)
	b := f.read_raw_at[u8](at)!
	at += sizeof(u8)
	c := f.read_raw_at[Color](at)!
	at += sizeof(Color)
	x := f.read_raw_at[Permissions](at)!
	at += sizeof(Permissions)
	f.close()

	assert p == another_point
	assert b == another_byte
	assert c == another_color
	assert x == another_permission
}

fn test_read_raw_at_negative_pos() {
	mut f := os.open_file(tfile, 'r')!
	if _ := f.read_raw_at[Point](u64(-1)) {
		assert false
	}
	f.read_raw_at[Point](u64(-1)) or { assert err.msg() == 'Invalid argument' }
	f.close()
}

fn test_seek() {
	mut f := os.open_file(tfile, 'w')!
	f.write_raw(another_point)!
	f.write_raw(another_byte)!
	f.write_raw(another_color)!
	f.write_raw(another_permission)!
	f.close()

	// println('> ${sizeof(Point)} ${sizeof(byte)} ${sizeof(Color)} ${sizeof(Permissions)}')
	f = os.open_file(tfile, 'r')!

	f.seek(i64(sizeof(Point)), .start)!
	assert f.tell()! == sizeof(Point)
	b := f.read_raw[u8]()!
	assert b == another_byte

	f.seek(i64(sizeof(Color)), .current)!
	x := f.read_raw[Permissions]()!
	assert x == another_permission

	f.close()
}

fn test_tell() {
	for size in 10 .. 30 {
		s := 'x'.repeat(size)
		os.write_file(tfile, s)!
		fs := os.file_size(tfile)
		assert int(fs) == size
		//
		mut f := os.open_file(tfile, 'r')!
		f.seek(-5, .end)!
		pos := f.tell()!
		f.seek(0, .start)!
		c1 := f.tell()!
		_ := f.read_bytes(8)
		c2 := f.tell()!
		assert c1 == 0
		assert c2 == 8
		f.close()
		// dump(pos)
		assert pos == size - 5
	}
}

fn test_reopen() {
	tfile1 := os.join_path_single(tfolder, 'tfile1')
	tfile2 := os.join_path_single(tfolder, 'tfile2')
	os.write_file(tfile1, 'Hello World!\nGood\r morning.\nBye 1.')!
	os.write_file(tfile2, 'Another file\nAnother line.\nBye 2.')!
	assert os.file_size(tfile1) > 0
	assert os.file_size(tfile2) > 0

	mut line_buffer := []u8{len: 1024}

	mut f2 := os.open(tfile2)!
	x := f2.read_bytes_with_newline(mut line_buffer)!
	assert !f2.eof()
	assert x > 0
	assert line_buffer#[..x].bytestr() == 'Another file\n'

	// Note: after this call, f2 should be using the file `tfile1`:
	f2.reopen(tfile1, 'r')!
	assert !f2.eof()

	z := f2.read(mut line_buffer) or { panic(err) }
	assert f2.eof()
	assert z > 0
	content := line_buffer#[..z].bytestr()
	// dump(content)
	assert content.starts_with('Hello World')
	assert content.ends_with('Bye 1.')
}

fn test_eof() {
	os.write_file(tfile, 'Hello World!\n')!

	mut f := os.open(tfile)!
	f.read_bytes(10)
	assert !f.eof()
	x := f.read_bytes(100)
	dump(x)
	dump(x.len)
	assert f.eof()
	f.close()
}

fn test_open_file_wb_ab() {
	os.rm(tfile) or {}
	mut wfile := os.open_file('text.txt', 'wb', 0o666)!
	wfile.write_string('hello')!
	wfile.close()
	assert os.read_file('text.txt')! == 'hello'

	mut afile := os.open_file('text.txt', 'ab', 0o666)!
	afile.write_string('hello')!
	afile.close()
	assert os.read_file('text.txt')! == 'hellohello'
}

fn test_open_append() {
	os.rm(tfile) or {}
	mut f1 := os.open_append(tfile)!
	f1.write_string('abc\n')!
	f1.close()
	assert os.read_lines(tfile)! == ['abc']

	mut f2 := os.open_append(tfile)!
	f2.write_string('abc\n')!
	f2.close()
	assert os.read_lines(tfile)! == ['abc', 'abc']

	mut f3 := os.open_append(tfile)!
	f3.write_string('def\n')!
	f3.close()
	assert os.read_lines(tfile)! == ['abc', 'abc', 'def']
}

fn test_open_file_on_chinese_windows() {
	$if windows {
		os.rm('中文.txt') or {}
		mut f1 := os.open_file('中文.txt', 'w+', 0x666) or { panic(err) }
		f1.write_string('test')!
		f1.close()

		assert os.read_file('中文.txt')! == 'test'
		assert os.file_size('中文.txt') == 4

		os.truncate('中文.txt', 2)!
		assert os.file_size('中文.txt') == 2
	}
}

fn test_open_file_crlf_binary_mode() {
	teststr := 'hello\r\n'
	fname := 'text.txt'

	mut wfile := os.open_file(fname, 'w', 0o666)!
	wfile.write_string(teststr)!
	wfile.close()

	mut fcont_w := os.read_file(fname)!

	os.rm(fname) or {}

	mut wbfile := os.open_file(fname, 'wb', 0o666)!
	wbfile.write_string(teststr)!
	wbfile.close()

	mut fcont_wb := os.read_file(fname)!

	os.rm(fname) or {}

	$if windows {
		assert fcont_w != teststr
	}

	assert fcont_wb == teststr
}

fn test_path_devnull() {
	dump(os.path_devnull)
	content := os.read_file(os.path_devnull)!
	// dump(content)
	// dump(content.len)

	os.write_file(os.path_devnull, 'something')!

	content_after := os.read_file(os.path_devnull)!
	// dump(content_after)
	// dump(content_after.len)
	assert content.len == 0
	assert content_after.len == 0
}

const some_lines_content = 'line 11\nline 22\nline33\n'

fn test_read_lines() {
	os.write_file(tfile, some_lines_content)!
	lines := os.read_lines(tfile)!
	assert lines == some_lines_content.split_into_lines()
}

fn test_write_lines() {
	wline1_file := os.join_path_single(tfolder, 'wline1.txt')
	wline2_file := os.join_path_single(tfolder, 'wline2.txt')
	os.write_file(wline1_file, some_lines_content)!
	lines := os.read_lines(wline1_file)!
	os.write_lines(wline2_file, lines)!
	c1 := os.read_file(wline1_file)!
	c2 := os.read_file(wline2_file)!
	assert c1 == c2
	assert c1.split_into_lines() == some_lines_content.split_into_lines()
}
