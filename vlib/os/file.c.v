module os

pub struct File {
	cfile voidptr // Using void* instead of FILE*
pub:
	fd int
pub mut:
	is_opened bool
}

struct FileInfo {
	name string
	size int
}

// open_file can be used to open or create a file with custom flags and permissions and returns a `File` object.
pub fn open_file(path string, mode string, options ...int) ?File {
	mut flags := 0
	for m in mode {
		match m {
			`w` { flags |= o_create | o_trunc }
			`a` { flags |= o_create | o_append }
			`r` { flags |= o_rdonly }
			`b` { flags |= o_binary }
			`s` { flags |= o_sync }
			`n` { flags |= o_nonblock }
			`c` { flags |= o_noctty }
			`+` { flags |= o_rdwr }
			else {}
		}
	}
	if mode == 'r+' {
		flags = o_rdwr
	}
	if mode == 'w' {
		flags = o_wronly | o_create | o_trunc
	}
	if mode == 'a' {
		flags = o_wronly | o_create | o_append
	}
	mut permission := 0o666
	if options.len > 0 {
		permission = options[0]
	}
	$if windows {
		if permission < 0o600 {
			permission = 0x0100
		} else {
			permission = 0x0100 | 0x0080
		}
	}
	mut p := path
	$if windows {
		p = path.replace('/', '\\')
	}
	fd := C.open(charptr(p.str), flags, permission)
	if fd == -1 {
		return error(posix_get_error_msg(C.errno))
	}
	cfile := C.fdopen(fd, charptr(mode.str))
	if isnil(cfile) {
		return error('Failed to open or create file "$path"')
	}
	return File{
		cfile: cfile
		fd: fd
		is_opened: true
	}
}

// open tries to open a file for reading and returns back a read-only `File` object.
pub fn open(path string) ?File {
	/*
	$if linux {
		$if !android {
			fd := C.syscall(sys_open, path.str, 511)
			if fd == -1 {
				return error('failed to open file "$path"')
			}
			return File{
				fd: fd
				is_opened: true
			}
		}
	}
	*/
	cfile := vfopen(path, 'rb') ?
	fd := fileno(cfile)
	return File{
		cfile: cfile
		fd: fd
		is_opened: true
	}
}

// create creates or opens a file at a specified location and returns a write-only `File` object.
pub fn create(path string) ?File {
	/*
	// NB: android/termux/bionic is also a kind of linux,
	// but linux syscalls there sometimes fail,
	// while the libc version should work.
	$if linux {
		$if !android {
			//$if macos {
			//	fd = C.syscall(398, path.str, 0x601, 0x1b6)
			//}
			//$if linux {
			fd = C.syscall(sys_creat, path.str, 511)
			//}
			if fd == -1 {
				return error('failed to create file "$path"')
			}
			file = File{
				fd: fd
				is_opened: true
			}
			return file
		}
	}
	*/
	cfile := vfopen(path, 'wb') ?
	fd := fileno(cfile)
	return File{
		cfile: cfile
		fd: fd
		is_opened: true
	}
}

// open_stdin - return an os.File for stdin, so that you can use .get_line on it too.
pub fn open_stdin() File {
	return File{
		fd: 0
		cfile: C.stdin
		is_opened: true
	}
}

// **************************** Write ops  ***************************
// write implements the Writer interface.
// It returns how many bytes were actually written.
pub fn (mut f File) write(buf []byte) ?int {
	if !f.is_opened {
		return error('file is not opened')
	}
	/*
	$if linux {
		$if !android {
			res := C.syscall(sys_write, f.fd, s.str, s.len)
			return res
		}
	}
	*/
	written := int(C.fwrite(buf.data, buf.len, 1, f.cfile))
	if written == 0 && buf.len != 0 {
		return error('0 bytes written')
	}
	return written
}

// writeln writes the string `s` into the file, and appends a \n character.
// It returns how many bytes were written, including the \n character.
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
	written := int(C.fwrite(s.str, s.len, 1, f.cfile))
	if written == 0 && s.len != 0 {
		return error('0 bytes written')
	}
	x := C.fputs('\n', f.cfile)
	if x < 0 {
		return error('could not add newline')
	}
	return (written + 1)
}

// write_string writes the string `s` into the file
// It returns how many bytes were actually written.
pub fn (mut f File) write_string(s string) ?int {
	if !f.is_opened {
		return error('file is not opened')
	}
	// TODO perf
	written := int(C.fwrite(s.str, s.len, 1, f.cfile))
	if written == 0 && s.len != 0 {
		return error('0 bytes written')
	}
	return written
}

// write_to implements the RandomWriter interface.
// It returns how many bytes were actually written.
// It resets the seek position to the end of the file.
pub fn (mut f File) write_to(pos int, buf []byte) ?int {
	if !f.is_opened {
		return error('file is not opened')
	}
	C.fseek(f.cfile, pos, C.SEEK_SET)
	res := int(C.fwrite(buf.data, 1, buf.len, f.cfile))
	if res == 0 && buf.len != 0 {
		return error('0 bytes written')
	}
	C.fseek(f.cfile, 0, C.SEEK_END)
	return res
}

// write_bytes writes `size` bytes to the file, starting from the address in `data`.
// NB: write_bytes is unsafe and should be used carefully, since if you pass invalid
// pointers to it, it will cause your programs to segfault.
[unsafe]
pub fn (mut f File) write_bytes(data voidptr, size int) int {
	return int(C.fwrite(data, 1, size, f.cfile))
}

// write_bytes_at writes `size` bytes to the file, starting from the address in `data`,
// at byte offset `pos`, counting from the start of the file (pos 0).
// NB: write_bytes_at is unsafe and should be used carefully, since if you pass invalid
// pointers to it, it will cause your programs to segfault.
[unsafe]
pub fn (mut f File) write_bytes_at(data voidptr, size int, pos int) int {
	C.fseek(f.cfile, pos, C.SEEK_SET)
	res := int(C.fwrite(data, 1, size, f.cfile))
	C.fseek(f.cfile, 0, C.SEEK_END)
	return res
}

// **************************** Read ops  ***************************
// read_bytes reads bytes from the beginning of the file.
// Utility method, same as .read_bytes_at(size, 0).
pub fn (f &File) read_bytes(size int) []byte {
	return f.read_bytes_at(size, 0)
}

// read_bytes_at reads `size` bytes at the given position in the file.
pub fn (f &File) read_bytes_at(size int, pos int) []byte {
	mut arr := []byte{len: size}
	nreadbytes := f.read_bytes_into(pos, mut arr) or {
		// return err
		return []
	}
	return arr[0..nreadbytes]
}

// read_bytes_into fills `buf` with bytes at the given position in the file.
// `buf` *must* have length greater than zero.
// Returns the number of read bytes, or an error.
pub fn (f &File) read_bytes_into(pos int, mut buf []byte) ?int {
	if buf.len == 0 {
		panic(@FN + ': `buf.len` == 0')
	}
	// Note: fseek errors if pos == os.file_size, which we accept
	C.fseek(f.cfile, pos, C.SEEK_SET)
	// errno is only set if fread fails, so clear it first to tell
	C.errno = 0
	nbytes := int(C.fread(buf.data, 1, buf.len, f.cfile))
	if C.errno != 0 {
		return error(posix_get_error_msg(C.errno))
	}
	$if debug {
		C.fseek(f.cfile, 0, C.SEEK_SET)
	}
	return nbytes
}

// read implements the Reader interface.
pub fn (f &File) read(mut buf []byte) ?int {
	if buf.len == 0 {
		return 0
	}
	C.errno = 0
	nbytes := int(C.fread(buf.data, 1, buf.len, f.cfile))
	if C.errno != 0 {
		return error(posix_get_error_msg(C.errno))
	}
	return nbytes
}

// read_at reads `buf.len` bytes starting at file byte offset `pos`, in `buf`.
pub fn (f &File) read_at(pos int, mut buf []byte) ?int {
	if buf.len == 0 {
		return 0
	}
	C.fseek(f.cfile, pos, C.SEEK_SET)
	C.errno = 0
	nbytes := int(C.fread(buf.data, 1, buf.len, f.cfile))
	if C.errno != 0 {
		return error(posix_get_error_msg(C.errno))
	}
	return nbytes
}

// **************************** Utility  ops ***********************
// flush writes any buffered unwritten data left in the file stream.
pub fn (mut f File) flush() {
	if !f.is_opened {
		return
	}
	C.fflush(f.cfile)
}

// write_str writes the bytes of a string into a file,
// *including* the terminating 0 byte.
pub fn (mut f File) write_str(s string) ? {
	if !f.is_opened {
		return error('file is closed')
	}
	f.write(s.bytes()) ?
}

// read_struct reads a single struct of type `T`
pub fn (mut f File) read_struct<T>(mut t T) ? {
	if !f.is_opened {
		return none
	}
	tsize := int(sizeof(*t))
	if tsize == 0 {
		return none
	}
	C.errno = 0
	nbytes := int(C.fread(t, 1, tsize, f.cfile))
	if C.errno != 0 {
		return error(posix_get_error_msg(C.errno))
	}
	if nbytes != tsize {
		return error_with_code('incomplete struct read', nbytes)
	}
}

// read_struct_at reads a single struct of type `T` at position specified in file
pub fn (mut f File) read_struct_at<T>(mut t T, pos int) ? {
	if !f.is_opened {
		return none
	}
	tsize := int(sizeof(*t))
	if tsize == 0 {
		return none
	}
	C.errno = 0
	C.fseek(f.cfile, pos, C.SEEK_SET)
	nbytes := int(C.fread(t, 1, tsize, f.cfile))
	C.fseek(f.cfile, 0, C.SEEK_END)
	if C.errno != 0 {
		return error(posix_get_error_msg(C.errno))
	}
	if nbytes != tsize {
		return error_with_code('incomplete struct read', nbytes)
	}
}

// read_raw reads and returns a single instance of type `T`
pub fn (mut f File) read_raw<T>() ?T {
	if !f.is_opened {
		return none
	}
	tsize := int(sizeof(T))
	if tsize == 0 {
		return none
	}
	C.errno = 0
	mut t := T{}
	nbytes := int(C.fread(&t, 1, tsize, f.cfile))
	if C.errno != 0 {
		return error(posix_get_error_msg(C.errno))
	}
	if nbytes != tsize {
		return error_with_code('incomplete struct read', nbytes)
	}
	return t
}

// read_raw_at reads and returns a single instance of type `T` starting at file byte offset `pos`
pub fn (mut f File) read_raw_at<T>(pos int) ?T {
	if !f.is_opened {
		return none
	}
	tsize := int(sizeof(T))
	if tsize == 0 {
		return none
	}
	C.errno = 0
	if C.fseek(f.cfile, pos, C.SEEK_SET) != 0 {
		return error(posix_get_error_msg(C.errno))
	}
	mut t := T{}
	nbytes := int(C.fread(&t, 1, tsize, f.cfile))
	if C.errno != 0 {
		return error(posix_get_error_msg(C.errno))
	}
	if C.fseek(f.cfile, 0, C.SEEK_END) != 0 {
		return error(posix_get_error_msg(C.errno))
	}
	if nbytes != tsize {
		return error_with_code('incomplete struct read', nbytes)
	}
	return t
}

// write_struct writes a single struct of type `T`
pub fn (mut f File) write_struct<T>(t &T) ? {
	if !f.is_opened {
		return error('file is not opened')
	}
	tsize := int(sizeof(T))
	if tsize == 0 {
		return error('struct size is 0')
	}
	C.errno = 0
	nbytes := int(C.fwrite(t, 1, tsize, f.cfile))
	if C.errno != 0 {
		return error(posix_get_error_msg(C.errno))
	}
	if nbytes != tsize {
		return error_with_code('incomplete struct write', nbytes)
	}
}

// write_struct_at writes a single struct of type `T` at position specified in file
pub fn (mut f File) write_struct_at<T>(t &T, pos int) ? {
	if !f.is_opened {
		return error('file is not opened')
	}
	tsize := int(sizeof(T))
	if tsize == 0 {
		return error('struct size is 0')
	}
	C.errno = 0
	C.fseek(f.cfile, pos, C.SEEK_SET)
	nbytes := int(C.fwrite(t, 1, tsize, f.cfile))
	C.fseek(f.cfile, 0, C.SEEK_END)
	if C.errno != 0 {
		return error(posix_get_error_msg(C.errno))
	}
	if nbytes != tsize {
		return error_with_code('incomplete struct write', nbytes)
	}
}

// TODO `write_raw[_at]` implementations are copy-pasted from `write_struct[_at]`

// write_raw writes a single instance of type `T`
pub fn (mut f File) write_raw<T>(t &T) ? {
	if !f.is_opened {
		return error('file is not opened')
	}
	tsize := int(sizeof(T))
	if tsize == 0 {
		return error('struct size is 0')
	}
	C.errno = 0
	nbytes := int(C.fwrite(t, 1, tsize, f.cfile))
	if C.errno != 0 {
		return error(posix_get_error_msg(C.errno))
	}
	if nbytes != tsize {
		return error_with_code('incomplete struct write', nbytes)
	}
}

// write_raw_at writes a single instance of type `T` starting at file byte offset `pos`
pub fn (mut f File) write_raw_at<T>(t &T, pos int) ? {
	if !f.is_opened {
		return error('file is not opened')
	}
	tsize := int(sizeof(T))
	if tsize == 0 {
		return error('struct size is 0')
	}
	if C.fseek(f.cfile, pos, C.SEEK_SET) != 0 {
		return error(posix_get_error_msg(C.errno))
	}
	nbytes := int(C.fwrite(t, 1, tsize, f.cfile))
	if C.errno != 0 {
		return error(posix_get_error_msg(C.errno))
	}
	if C.fseek(f.cfile, 0, C.SEEK_END) != 0 {
		return error(posix_get_error_msg(C.errno))
	}
	if nbytes != tsize {
		return error_with_code('incomplete struct write', nbytes)
	}
}
