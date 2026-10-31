module io

import os

// seek_to repositions any Seeker; used to prove os.File satisfies Seeker.
fn seek_to(mut s Seeker, pos i64) ! {
	s.seek(pos, .start)!
}

fn test_seeker_with_os_file() {
	root := os.join_path(os.vtmp_dir(), 'io_seeker_${os.getpid()}')
	// mkdir fails if the directory already exists, so this test only owns a new fixture.
	os.mkdir(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	path := os.join_path(root, 'input.txt')
	os.write_file(path, 'hello')!
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
