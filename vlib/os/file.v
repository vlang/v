module os

pub struct File {
	cfile  voidptr // Using void* instead of FILE*
pub:
	fd     int
pub mut:
	is_opened bool
}

struct FileInfo {
	name string
	size int
}

[deprecated]
pub fn (f File) is_opened() bool {
	eprintln('warning: `file.is_opened()` has been deprecated, use `file.is_opened` instead')
	return f.is_opened
}

// **************************** Write ops  ***************************
pub fn (mut f File) write(s string) {
	if !f.is_opened {
		return
	}
	/*
	$if linux {
		$if !android {
			C.syscall(sys_write, f.fd, s.str, s.len)
			return
		}
	}
	*/
	C.fwrite(s.str, s.len, 1, f.cfile)
}

pub fn (mut f File) writeln(s string) {
	if !f.is_opened {
		return
	}
	/*
	$if linux {
		$if !android {
			snl := s + '\n'
			C.syscall(sys_write, f.fd, snl.str, snl.len)
			return
		}
	}
	*/
	// TODO perf
	C.fwrite(s.str, s.len, 1, f.cfile)
	C.fputs('\n', f.cfile)
}

pub fn (mut f File) write_bytes(data voidptr, size int) int {
	return C.fwrite(data, 1, size, f.cfile)
}

pub fn (mut f File) write_bytes_at(data voidptr, size, pos int) int {
	C.fseek(f.cfile, pos, C.SEEK_SET)
	res := C.fwrite(data, 1, size, f.cfile)
	C.fseek(f.cfile, 0, C.SEEK_END)
	return res
}

// **************************** Read ops  ***************************
// read_bytes reads an amount of bytes from the beginning of the file
pub fn (f &File) read_bytes(size int) []byte {
	return f.read_bytes_at(size, 0)
}

// read_bytes_at reads an amount of bytes at the given position in the file
pub fn (f &File) read_bytes_at(size, pos int) []byte {
	//mut arr := [`0`].repeat(size)
	mut arr := []byte{ len:size }
	C.fseek(f.cfile, pos, C.SEEK_SET)
	nreadbytes := C.fread(arr.data, 1, size, f.cfile)
	C.fseek(f.cfile, 0, C.SEEK_SET)
	return arr[0..nreadbytes]
}

// **************************** Utility  ops ***********************
// write any unwritten data in stream's buffer
pub fn (mut f File) flush() {
	if !f.is_opened {
		return
	}
	C.fflush(f.cfile)
}
