import os

struct Point {
	x f64
	y f64
	z f64
}

const unit_point = Point{1.0, 1.0, 1.0}

const tfolder = os.join_path(os.temp_dir(), 'os_file_test')

const tfile = os.join_path(tfolder, 'test_file')

const another_point = Point{0.25, 2.25, 6.25}

fn testsuite_begin() {
	os.rmdir_all(tfolder) or { }
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
