import os

struct Sample {
	a voidptr
	b int
	c bool
}

fn mk(path string) !Sample {
	cfile := os.vfopen(path, 'wb')!
	fd := os.fileno(cfile)
	return Sample{
		a: cfile
		b: fd
		c: true
	}
}

fn main() {
	s := mk('/tmp/v2_result_postfix_unwrap.txt') or {
		println('err')
		return
	}
	println(s.b > 0)
	println(s.c)
}

// v2-clobber-test
