module io

import os

// seek_to repositions any Seeker; used to prove os.File satisfies Seeker.
fn seek_to(mut s Seeker, pos i64) ! {
	s.seek(pos, .start)!
}

fn test_seeker_with_os_file() {
	path := os.join_path(os.temp_dir(), 'v_io_seeker_test.txt')
	os.write_file(path, 'hello') or { panic(err) }
	defer {
		os.rm(path) or {}
	}
	mut f := os.open(path)!
	defer {
		f.close()
	}
	// os.File passed as Seeker: conformance is checked by the compiler.
	seek_to(mut f, 1)!
	mut buf := []u8{len: 1}
	n := f.read(mut buf)!
	assert n == 1
	assert buf == [u8(`e`)]
}
