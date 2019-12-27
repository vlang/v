module os2

#include <fcntl.h>
struct File {
	fd int
}

fn C.perror(charptr)


fn C.open(byteptr, int, int) int


fn C.write(voidptr, byteptr, int) int


fn C.close(int) int


pub fn create(path string) ?File {
	fd := C.open(path.str, C.O_CREAT | C.O_TRUNC | C.O_WRONLY, 0644) // 511
	if fd == -1 {
		return error('failed to create "$path":')
		// os.print_c_errno()
	}
	return File{
		fd}
}

pub fn (f File) writeln(s string) {
	ss := s + '\n'
	ret := C.write(f.fd, ss.str, s.len + 1)
	if ret == -1 {
		C.perror('failed to write')
	}
}

pub fn (f File) close() {
	C.close(f.fd)
}
