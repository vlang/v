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

fn testsuite_begin() {
	os.rmdir_all(tfolder) or {}
	assert !os.is_dir(tfolder)
	os.mkdir_all(tfolder) or { panic(err) }
	os.chdir(tfolder)
	assert os.is_dir(tfolder)
}

fn testsuite_end() {
	os.chdir(os.wd_at_startup)
	os.rmdir_all(tfolder) or { panic(err) }
	assert !os.is_dir(tfolder)
}

fn test_write_struct() {
	size_of_point := int(sizeof(Point))
	mut f := os.open_file(tfile, 'w') or { panic(err) }
	f.write_struct(another_point) or { panic(err) }
	f.close()
	x := os.read_file(tfile) or { panic(err) }
	y := unsafe { byteptr(memdup(&another_point, size_of_point)).vstring_with_len(size_of_point) }
	assert x == y
	$if debug {
		eprintln(x.bytes())
		eprintln(y.bytes())
	}
}

fn test_write_struct_at() {
	mut f := os.open_file(tfile, 'w') or { panic(err) }
	f.write_struct(extended_point) or { panic(err) }
	f.write_struct_at(another_point, 3) or { panic(err) }
	f.close()
	f = os.open_file(tfile, 'r') or { panic(err) }
	mut p := Point{}
	f.read_struct_at(mut p, 3) or { panic(err) }
	f.close()

	assert p == another_point
}

fn test_read_struct() {
	mut f := os.open_file(tfile, 'w') or { panic(err) }
	f.write_struct(another_point) or { panic(err) }
	f.close()

	f = os.open_file(tfile, 'r') or { panic(err) }
	mut p := Point{}
	f.read_struct(mut p) or { panic(err) }
	f.close()

	assert p == another_point
}

fn test_read_struct_at() {
	mut f := os.open_file(tfile, 'w') or { panic(err) }
	f.write([byte(1), 2, 3]) or { panic(err) }
	f.write_struct(another_point) or { panic(err) }
	f.close()
	f = os.open_file(tfile, 'r') or { panic(err) }
	mut p := Point{}
	f.read_struct_at(mut p, 3) or { panic(err) }
	f.close()

	assert p == another_point
}

/*
fn test_write_any() {
	size_of_point := int(sizeof(Point))
	mut f := os.open_file(tfile, 'w') or { panic(err) }
	f.write_any(another_point) or { panic(err) }
	f.close()
	x := os.read_file(tfile) or { panic(err) }
	y := unsafe { byteptr(memdup(&another_point, size_of_point)).vstring_with_len(size_of_point) }
	assert x == y
	$if debug {
		eprintln(x.bytes())
		eprintln(y.bytes())
	}
}

fn test_write_any_at() {
	mut f := os.open_file(tfile, 'w') or { panic(err) }
	f.write_any(extended_point) or { panic(err) }
	f.write_any_at(another_point, 3) or { panic(err) }
	f.close()
	f = os.open_file(tfile, 'r') or { panic(err) }
	mut p := Point{}
	f.read_struct_at(mut p, 3) or { panic(err) }
	f.close()

	assert p == another_point
}
*/

fn test_read_any() {
	mut f := os.open_file(tfile, 'w') or { panic(err) }
	f.write_struct(another_point) or { panic(err) }
	f.write_struct(another_byte) or { panic(err) }
	f.write_struct(another_color) or { panic(err) }
	f.write_struct(another_permission) or { panic(err) }
	f.close()
	f = os.open_file(tfile, 'r') or { panic(err) }
	p := f.read_any<Point>() or { panic(err) }
	b := f.read_any<byte>() or { panic(err) }
	c := f.read_any<Color>() or { panic(err) }
	x := f.read_any<Permissions>() or { panic(err) }
	f.close()

	assert p == another_point
	assert b == another_byte
	assert c == another_color
	assert x == another_permission
}

fn test_read_any_at() {
	mut f := os.open_file(tfile, 'w') or { panic(err) }
	f.write([byte(1), 2, 3]) or { panic(err) }
	f.write_struct(another_point) or { panic(err) }
	f.write_struct(another_byte) or { panic(err) }
	f.write_struct(another_color) or { panic(err) }
	f.write_struct(another_permission) or { panic(err) }
	f.close()
	f = os.open_file(tfile, 'r') or { panic(err) }
	mut at := 3
	p := f.read_any_at<Point>(at) or { panic(err) }
	at += int(sizeof(Point))
	b := f.read_any_at<byte>(at) or { panic(err) }
	at += int(sizeof(byte))
	c := f.read_any_at<Color>(at) or { panic(err) }
	at += int(sizeof(Color))
	x := f.read_any_at<Permissions>(at) or { panic(err) }
	at += int(sizeof(Permissions))
	f.close()

	assert p == another_point
	assert b == another_byte
	assert c == another_color
	assert x == another_permission
}
