module os

pub struct File {
	cfile     voidptr // Using void* instead of FILE*
pub:
	fd        int
pub mut:
	is_opened bool
}

struct FileInfo {
	name string
	size int
}

[deprecated]
pub fn (f File) is_opened() bool {
	eprintln('warning: `File.is_opened()` has been deprecated, use `File.is_opened` instead')
	return f.is_opened
}

// **************************** Write ops  ***************************
// write implements the Writer interface
pub fn (mut f File) write(buf []byte) ?int {
	if !f.is_opened {
		return error('file is not opened')
	}
	/*
	$if linux {
		$if !android {
			C.syscall(sys_write, f.fd, s.str, s.len)
			return
		}
	}
	*/
	written := C.fwrite(buf.data, buf.len, 1, f.cfile)
	if written == 0 && buf.len != 0 {
		return error('0 bytes written')
	}
	return written
}

pub fn (mut f File) writeln(s string) ?int {
	if !f.is_opened {
		return error('file is not opened')
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
	written := C.fwrite(s.str, s.len, 1, f.cfile)
	if written == 0 && s.len != 0 {
		return error('0 bytes written')
	}
	x := C.fputs('\n', f.cfile)
	if x < 0 {
		return error('could not add newline')
	}
	return (written + 1)
}

// write_to implements the RandomWriter interface
pub fn (mut f File) write_to(pos int, buf []byte) ?int {
	C.fseek(f.cfile, pos, C.SEEK_SET)
	res := C.fwrite(buf.data, 1, buf.len, f.cfile)
	C.fseek(f.cfile, 0, C.SEEK_END)
	return res
}

[deprecated]
pub fn (mut f File) write_bytes(data voidptr, size int) int {
	eprintln('warning `File.write_bytes()` has been deprecated, use `File.write` instead')
	return C.fwrite(data, 1, size, f.cfile)
}

[deprecated]
pub fn (mut f File) write_bytes_at(data voidptr, size int, pos int) int {
	eprintln('warning `File.write_bytes_at()` has been deprecated, use `File.write_at` instead')
	C.fseek(f.cfile, pos, C.SEEK_SET)
	res := C.fwrite(data, 1, size, f.cfile)
	C.fseek(f.cfile, 0, C.SEEK_END)
	return res
}

// **************************** Read ops  ***************************
// read_bytes reads bytes from the beginning of the file
[deprecated]
pub fn (f &File) read_bytes(size int) []byte {
	eprintln('warning `File.read_bytes()` has been deprecated, use `File.read` instead')
	return f.read_bytes_at(size, 0)
}

// read_bytes_at reads bytes at the given position in the file
[deprecated]
pub fn (f &File) read_bytes_at(size int, pos int) []byte {
	eprintln('warning `File.read_bytes_at()` has been deprecated, use `File.read_at` instead')
	mut arr := []byte{len: size}
	nreadbytes := f.read_bytes_into(pos, mut arr) or {
		// return err
		return []
	}
	return arr[0..nreadbytes]
}

// read_bytes_from fills `buf` with bytes at the given position in the file.
// `buf` must have length greater than zero.
// Returns number of bytes read or an error.
[deprecated]
pub fn (f &File) read_bytes_into(pos int, mut buf []byte) ?int {
	eprintln('warning `File.read_bytes_into()` has been deprecated, use `File.read_from_into` instead')
	if buf.len == 0 {
		panic(@FN + ': `buf.len` == 0')
	}
	// Note: fseek errors if pos == os.file_size, which we accept
	C.fseek(f.cfile, pos, C.SEEK_SET)
	// errno is only set if fread fails, so clear it first to tell
	C.errno = 0
	nbytes := C.fread(buf.data, 1, buf.len, f.cfile)
	if C.errno != 0 {
		return error(posix_get_error_msg(C.errno))
	}
	$if debug {
		C.fseek(f.cfile, 0, C.SEEK_SET)
	}
	return nbytes
}

// read implements the Reader interface
pub fn (f &File) read(mut buf []byte) ?int {
	if buf.len == 0 {
		return 0
	}
	C.errno = 0
	nbytes := C.fread(buf.data, 1, buf.len, f.cfile)
	if C.errno != 0 {
		return error(posix_get_error_msg(C.errno))
	}
	return nbytes
}

// read_at reads buf.len bytes from pos in the file
pub fn (f &File) read_at(pos int, mut buf []byte) ?int {
	if buf.len == 0 {
		return 0
	}
	C.fseek(f.cfile, pos, C.SEEK_SET)
	C.errno = 0
	nbytes := C.fread(buf.data, 1, buf.len, f.cfile)
	if C.errno != 0 {
		return error(posix_get_error_msg(C.errno))
	}
	return nbytes
}

// **************************** Utility  ops ***********************
// flush writes any unwritten data in stream's buffer
pub fn (mut f File) flush() {
	if !f.is_opened {
		return
	}
	C.fflush(f.cfile)
}

// open_stdin - return an os.File for stdin, so that you can use .get_line on it too.
pub fn open_stdin() File {
	return File{
		fd: 0
		cfile: C.stdin
		is_opened: true
	}
}

// File.get_line - get a single line from the file. NB: the ending newline is *included*.
[deprecated]
pub fn (mut f File) get_line() ?string {
	eprintln('File.get_line() is deprecated... Use a BufferedReader instead')
	if !f.is_opened {
		return error('file is closed')
	}
	return error('use io.new_buffered_reader')
	/*
	mut reader := io.new_buffered_reader({
		reader: io.make_reader(f)
	})
	return reader.read_line()
	*/
}

pub fn (mut f File) write_str(s string) ? {
	if !f.is_opened {
		return error('file is closed')
	}
	f.write(s.bytes()) ?
}
