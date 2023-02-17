module builtin

pub fn print(s string) {
	elm := CIOVec{buf: s.str, len: usize(s.len)}
	
	WASM.fd_write(1, &elm, 1, -1)
}

pub fn panic(s string) {
	//println(s)
	//_ := *&u8(0)
}