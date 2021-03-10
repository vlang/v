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
	tfolder = os.join_path(os.temp_dir(), 'os_file_test')
	tfile   = os.join_path(tfolder, 'test_file')
)

fn testsuite_begin() ? {
	os.rmdir_all(tfolder) or {}
	assert !os.is_dir(tfolder)
	os.mkdir_all(tfolder) ?
	os.chdir(tfolder)
	assert os.is_dir(tfolder)
}

fn testsuite_end() ? {
	os.chdir(os.wd_at_startup)
	os.rmdir_all(tfolder) ?
	assert !os.is_dir(tfolder)
}

fn test_write_struct() ? {
	os.rm(tfile) or {} // FIXME This is a workaround for macos, because the file isn't truncated when open with 'w'
	size_of_point := int(sizeof(Point))
	mut f := os.open_file(tfile, 'w') ?
	f.write_struct(another_point) ?
	f.close()
	x := os.read_file(tfile) ?
	y := unsafe { byteptr(memdup(&another_point, size_of_point)).vstring_with_len(size_of_point) }
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
	y := unsafe { byteptr(memdup(&another_point, size_of_point)).vstring_with_len(size_of_point) }
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
	f.write_raw_at(another_point, -234) or { assert err.msg == 'Invalid argument' }
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
	mut at := 3
	p := f.read_raw_at<Point>(at) ?
	at += int(sizeof(Point))
	b := f.read_raw_at<byte>(at) ?
	at += int(sizeof(byte))
	c := f.read_raw_at<Color>(at) ?
	at += int(sizeof(Color))
	x := f.read_raw_at<Permissions>(at) ?
	at += int(sizeof(Permissions))
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
	f.read_raw_at<Point>(-234) or { assert err.msg == 'Invalid argument' }
	f.close()
}
