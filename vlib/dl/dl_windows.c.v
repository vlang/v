module dl

pub const (
	RTLD_NOW = 0
	DL_EXT = '.dll'
)

fn C.LoadLibraryA(libfilename byteptr) voidptr
fn C.GetProcAddress(handle voidptr, procname byteptr) voidptr

pub fn open(filename string, flags int) voidptr {
	res := C.LoadLibraryA(filename.str)
	return res
}

pub fn sym(handle voidptr, symbol string) voidptr {
	return C.GetProcAddress(handle, symbol.str)
}
