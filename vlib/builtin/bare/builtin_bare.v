module builtin

pub fn syscall5(number, arg1, arg2, arg3, arg4, arg5 voidptr) voidptr

// TODO no pub => error
pub fn write(fd int, data voidptr, nbytes int) int {
	return syscall5(
          1, // SYS_write
          fd,
          data,
          nbytes,
          0, // ignored
          0  // ignored
	)
}

pub fn println(s string) {
	write(1, s.str, s.len)
}

pub fn panic(s string) {
	write(1, s.str, s.len)
}

pub fn malloc(n int) voidptr {
	return syscall5(0,0,0,0,0,0)
	
}
