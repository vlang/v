module builtin

pub fn print(s string) {
	elm := CIOVec{
		buf: s.str
		len: usize(s.len)
	}

	WASM.fd_write(1, &elm, 1, -1)
}

pub fn println(s string) {
	elm := [CIOVec{
		buf: s.str
		len: usize(s.len)
	}, CIOVec{
		buf: c'\n'
		len: 1
	}]!

	WASM.fd_write(1, &elm[0], 2, -1)
}

pub fn eprint(s string) {
	elm := CIOVec{
		buf: s.str
		len: usize(s.len)
	}

	WASM.fd_write(2, &elm, 1, -1)
}

pub fn eprintln(s string) {
	elm := [CIOVec{
		buf: s.str
		len: usize(s.len)
	}, CIOVec{
		buf: c'\n'
		len: 1
	}]!

	WASM.fd_write(2, &elm[0], 2, -1)
}

[noreturn]
pub fn exit(code int) {
	WASM.proc_exit(code)
}

[noreturn]
pub fn panic(s string) {
	eprint('V panic: ')
	eprintln(s)
	exit(1)
}
