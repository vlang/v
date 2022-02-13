import os

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

[flag]
enum Permissions {
	read
	write
	execute
}

const (
	unit_point         = Point{1.0, 1.0, 1.0}
	another_point      = Point{0.25, 2.25, 6.25}
	extended_point     = Extended_Point{1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0}
	another_byte       = byte(123)
	another_color      = Color.red
	another_permission = Permissions.read | .write
)

const (
	tfolder = os.join_path_single(os.temp_dir(), 'os_file_test')
	tfile   = os.join_path_single(tfolder, 'test_file')
)

fn testsuite_begin() ? {
	os.rmdir_all(tfolder) or {}
	assert !os.is_dir(tfolder)
	os.mkdir_all(tfolder) ?
	os.chdir(tfolder) ?
	assert os.is_dir(tfolder)
}

fn testsuite_end() ? {
	os.chdir(os.wd_at_startup) ?
	os.rmdir_all(tfolder) ?
	assert !os.is_dir(tfolder)
}

// test_read_bytes_into_newline_text tests reading text from a file with newlines.
// This test simulates reading a larger text file step by step into a buffer and
// returning on each newline, even before the buffer is full, and reaching EOF before
// the buffer is completely filled.
fn test_read_bytes_into_newline_text() ? {
	mut f := os.open_file(tfile, 'w') ?
	f.write_string('Hello World!\nGood\r morning.') ?
	f.close()

	f = os.open_file(tfile, 'r') ?
	mut buf := []byte{len: 8}

	n0 := f.read_bytes_into_newline(mut buf) ?
	assert n0 == 8

	n1 := f.read_bytes_into_newline(mut buf) ?
	assert n1 == 5

	n2 := f.read_bytes_into_newline(mut buf) ?
	assert n2 == 8

	n3 := f.read_bytes_into_newline(mut buf) ?
	assert n3 == 6

	f.close()
}

// test_read_bytes_into_newline_binary tests reading a binary file with NUL bytes.
// This test simulates the scenario when a byte stream is read and a newline byte
// appears in that stream and an EOF occurs before the buffer is full.
fn test_read_bytes_into_newline_binary() ? {
	os.rm(tfile) or {} // FIXME This is a workaround for macos, because the file isn't truncated when open with 'w'
	mut bw := []byte{len: 15}
	bw[9] = 0xff
	bw[12] = 10 // newline

	n0_bytes := bw[0..10]
	n1_bytes := bw[10..13]
	n2_bytes := bw[13..]

	mut f := os.open_file(tfile, 'w') ?
	f.write(bw) ?
	f.close()

	f = os.open_file(tfile, 'r') ?
	mut buf := []byte{len: 10}

	n0 := f.read_bytes_into_newline(mut buf) ?
	assert n0 == 10
	assert buf[..n0] == n0_bytes

	n1 := f.read_bytes_into_newline(mut buf) ?
	assert n1 == 3
	assert buf[..n1] == n1_bytes

	n2 := f.read_bytes_into_newline(mut buf) ?
	assert n2 == 2
	assert buf[..n2] == n2_bytes
	f.close()
}

// test_read_eof_last_read_partial_buffer_fill tests that when reading a file
// the end-of-file is detected and results in a none error being returned. This
// test simulates file reading where the end-of-file is reached inside an fread
// containing data.
fn test_read_eof_last_read_partial_buffer_fill() ? {
	mut f := os.open_file(tfile, 'w') ?
	bw := []byte{len: 199, init: 5}
	f.write(bw) ?
	f.close()

	f = os.open_file(tfile, 'r') ?
	mut br := []byte{len: 100}
	// Read first 100 bytes of 199 byte file, should fill buffer with no error.
	n0 := f.read(mut br) ?
	assert n0 == 100
	// Read remaining 99 bytes of 199 byte file, should fill buffer with no
	// error, even though end-of-file was reached.
	n1 := f.read(mut br) ?
	assert n1 == 99
	// Read again, end-of-file was previously reached so should return none
	// error.
	if _ := f.read(mut br) {
		// This is not intended behavior because the read function should
		// not return a number of bytes read when end-of-file is reached.
		assert false
	} else {
		// Expect none to have been returned when end-of-file.
		assert err is none
	}
	f.close()
}

// test_read_eof_last_read_full_buffer_fill tests that when reading a file the
// end-of-file is detected and results in a none error being returned. This test
// simulates file reading where the end-of-file is reached at the beinning of an
// fread that returns no data.
fn test_read_eof_last_read_full_buffer_fill() ? {
	mut f := os.open_file(tfile, 'w') ?
	bw := []byte{len: 200, init: 5}
	f.write(bw) ?
	f.close()

	f = os.open_file(tfile, 'r') ?
	mut br := []byte{len: 100}
	// Read first 100 bytes of 200 byte file, should fill buffer with no error.
	n0 := f.read(mut br) ?
	assert n0 == 100
	// Read remaining 100 bytes of 200 byte file, should fill buffer with no
	// error. The end-of-file isn't reached yet, but there is no more data.
	n1 := f.read(mut br) ?
	assert n1 == 100
	// Read again, end-of-file was previously reached so should return none
	// error.
	if _ := f.read(mut br) {
		// This is not intended behavior because the read function should
		// not return a number of bytes read when end-of-file is reached.
		assert false
	} else {
		// Expect none to have been returned when end-of-file.
		assert err is none
	}
	f.close()
}

fn test_write_struct() ? {
	os.rm(tfile) or {} // FIXME This is a workaround for macos, because the file isn't truncated when open with 'w'
	size_of_point := int(sizeof(Point))
	mut f := os.open_file(tfile, 'w') ?
	f.write_struct(another_point) ?
	f.close()
	x := os.read_file(tfile) ?
	pcopy := unsafe { &byte(memdup(&another_point, size_of_point)) }
	y := unsafe { pcopy.vstring_with_len(size_of_point) }
	assert x == y
	$if debug {
		eprintln(x.bytes())
		eprintln(y.bytes())
	}
}

fn test_write_struct_at() ? {
	mut f := os.open_file(tfile, 'w') ?
	f.write_struct(extended_point) ?
	f.write_struct_at(another_point, 3) ?
	f.close()
	f = os.open_file(tfile, 'r') ?
	mut p := Point{}
	f.read_struct_at(mut p, 3) ?
	f.close()

	assert p == another_point
}

fn test_read_struct() ? {
	mut f := os.open_file(tfile, 'w') ?
	f.write_struct(another_point) ?
	f.close()

	f = os.open_file(tfile, 'r') ?
	mut p := Point{}
	f.read_struct(mut p) ?
	f.close()

	assert p == another_point
}

fn test_read_struct_at() ? {
	mut f := os.open_file(tfile, 'w') ?
	f.write([byte(1), 2, 3]) ?
	f.write_struct(another_point) ?
	f.close()
	f = os.open_file(tfile, 'r') ?
	mut p := Point{}
	f.read_struct_at(mut p, 3) ?
	f.close()

	assert p == another_point
}

fn test_write_raw() ? {
	os.rm(tfile) or {} // FIXME This is a workaround for macos, because the file isn't truncated when open with 'w'
	size_of_point := int(sizeof(Point))
	mut f := os.open_file(tfile, 'w') ?
	f.write_raw(another_point) ?
	f.close()
	x := os.read_file(tfile) ?
	pcopy := unsafe { &byte(memdup(&another_point, size_of_point)) }
	y := unsafe { pcopy.vstring_with_len(size_of_point) }
	assert x == y
	$if debug {
		eprintln(x.bytes())
		eprintln(y.bytes())
	}
}

fn test_write_raw_at() ? {
	mut f := os.open_file(tfile, 'w') ?
	f.write_raw(extended_point) ?
	f.write_raw_at(another_point, 3) ?
	f.close()
	f = os.open_file(tfile, 'r') ?
	mut p := Point{}
	f.read_struct_at(mut p, 3) ?
	f.close()

	assert p == another_point
}

fn test_write_raw_at_negative_pos() ? {
	mut f := os.open_file(tfile, 'w') ?
	if _ := f.write_raw_at(another_point, -1) {
		assert false
	}
	f.write_raw_at(another_point, -234) or { assert err.msg() == 'Invalid argument' }
	f.close()
}

fn test_read_raw() ? {
	mut f := os.open_file(tfile, 'w') ?
	f.write_raw(another_point) ?
	f.write_raw(another_byte) ?
	f.write_raw(another_color) ?
	f.write_raw(another_permission) ?
	f.close()
	f = os.open_file(tfile, 'r') ?
	p := f.read_raw<Point>() ?
	b := f.read_raw<byte>() ?
	c := f.read_raw<Color>() ?
	x := f.read_raw<Permissions>() ?
	f.close()

	assert p == another_point
	assert b == another_byte
	assert c == another_color
	assert x == another_permission
}

fn test_read_raw_at() ? {
	mut f := os.open_file(tfile, 'w') ?
	f.write([byte(1), 2, 3]) ?
	f.write_raw(another_point) ?
	f.write_raw(another_byte) ?
	f.write_raw(another_color) ?
	f.write_raw(another_permission) ?
	f.close()
	f = os.open_file(tfile, 'r') ?
	mut at := u64(3)
	p := f.read_raw_at<Point>(at) ?
	at += sizeof(Point)
	b := f.read_raw_at<byte>(at) ?
	at += sizeof(byte)
	c := f.read_raw_at<Color>(at) ?
	at += sizeof(Color)
	x := f.read_raw_at<Permissions>(at) ?
	at += sizeof(Permissions)
	f.close()

	assert p == another_point
	assert b == another_byte
	assert c == another_color
	assert x == another_permission
}

fn test_read_raw_at_negative_pos() ? {
	mut f := os.open_file(tfile, 'r') ?
	if _ := f.read_raw_at<Point>(-1) {
		assert false
	}
	f.read_raw_at<Point>(-234) or { assert err.msg() == 'Invalid argument' }
	f.close()
}

fn test_seek() ? {
	mut f := os.open_file(tfile, 'w') ?
	f.write_raw(another_point) ?
	f.write_raw(another_byte) ?
	f.write_raw(another_color) ?
	f.write_raw(another_permission) ?
	f.close()

	// println('> ${sizeof(Point)} ${sizeof(byte)} ${sizeof(Color)} ${sizeof(Permissions)}')
	f = os.open_file(tfile, 'r') ?
	//
	f.seek(i64(sizeof(Point)), .start) ?
	assert f.tell() ? == sizeof(Point)
	b := f.read_raw<byte>() ?
	assert b == another_byte

	f.seek(i64(sizeof(Color)), .current) ?
	x := f.read_raw<Permissions>() ?
	assert x == another_permission
	//
	f.close()
}

fn test_tell() ? {
	for size in 10 .. 30 {
		s := 'x'.repeat(size)
		os.write_file(tfile, s) ?
		fs := os.file_size(tfile)
		assert int(fs) == size
		//
		mut f := os.open_file(tfile, 'r') ?
		f.seek(-5, .end) ?
		pos := f.tell() ?
		f.close()
		// dump(pos)
		assert pos == size - 5
	}
}
