fn syscall5(number, arg1, arg2, arg3, arg4, arg5 voidptr) voidptr

fn write(fd int, data voidptr, nbytes u64) int {
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
C.write(1, "hallo\n", 6)
}
