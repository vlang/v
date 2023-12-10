module builtin

// print prints a message to stdout. Unlike `println` stdout is not automatically flushed.
pub fn print(s string) {
	elm := CIOVec{
		buf: s.str
		len: usize(s.len)
	}

	WASM.fd_write(1, &elm, 1, 0)
}

// println prints a message with a line end, to stdout.
pub fn println(s string) {
	elm := [CIOVec{
		buf: s.str
		len: usize(s.len)
	}, CIOVec{
		buf: c'\n'
		len: 1
	}]!

	WASM.fd_write(1, &elm[0], 2, 0)
}

// eprint prints a message to stderr.
pub fn eprint(s string) {
	elm := CIOVec{
		buf: s.str
		len: usize(s.len)
	}

	WASM.fd_write(2, &elm, 1, 0)
}

// eprintln prints a message with a line end, to stderr.
pub fn eprintln(s string) {
	elm := [CIOVec{
		buf: s.str
		len: usize(s.len)
	}, CIOVec{
		buf: c'\n'
		len: 1
	}]!

	WASM.fd_write(2, &elm[0], 2, 0)
}

// exit terminates execution immediately and returns exit `code` to the shell.
@[noreturn]
pub fn exit(code int) {
	WASM.proc_exit(code)
}

// panic prints a nice error message, then exits the process with exit code of 1.
@[noreturn]
pub fn panic(s string) {
	eprint('V panic: ')
	eprintln(s)
	exit(1)
}
