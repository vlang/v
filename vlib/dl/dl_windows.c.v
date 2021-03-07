module dl

pub const (
	rtld_now  = 0
	rtld_lazy = 0
)

fn C.LoadLibrary(libfilename &u16) voidptr

fn C.GetProcAddress(handle voidptr, procname byteptr) voidptr

fn C.FreeLibrary(handle voidptr) bool

// open loads a given module into the address space of the calling process.
pub fn open(filename string, flags int) voidptr {
	res := C.LoadLibrary(filename.to_wide())
	return res
}

// close frees the loaded a given module.
pub fn close(handle voidptr) bool {
	return C.FreeLibrary(handle)
}

// sym returns an address of an exported function or variable from a given module.
pub fn sym(handle voidptr, symbol string) voidptr {
	return C.GetProcAddress(handle, symbol.str)
}
