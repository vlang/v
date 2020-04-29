module dl

pub const (
	RTLD_NOW = 0
	DL_EXT = '.dll'
)

fn C.LoadLibrary(libfilename C.LPCWSTR) voidptr
fn C.GetProcAddress(handle voidptr, procname C.LPCSTR) voidptr
fn C.FreeLibrary(handle voidptr) bool

pub fn open(filename string, flags int) voidptr {
	res := C.LoadLibrary(filename.to_wide())
	return res
}

pub fn close(handle voidptr) bool {
	return C.FreeLibrary(handle)
}

pub fn sym(handle voidptr, symbol string) voidptr {
	return C.GetProcAddress(handle, symbol.str)
}
