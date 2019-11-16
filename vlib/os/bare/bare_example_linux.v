fn syscall5(number, arg1, arg2, arg3, arg4, arg5 voidptr) voidptr

fn write(fd int, data voidptr, nbytes int) int {
	return 	 syscall5(
          1, // SYS_write
           fd,
            data,
            nbytes,
            0, // ignored
            0  // ignored
        )
}

fn main() {
	write(1, c'hello\n', 6)
	s := 'test string'
	write(1, s.str, s.len)
	a := s[0]
}

