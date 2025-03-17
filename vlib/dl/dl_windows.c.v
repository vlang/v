module dl

pub const rtld_now = 0
pub const rtld_lazy = 0
pub const rtld_global = 0
pub const rtld_local = 0
pub const rtld_nodelete = 0
pub const rtld_noload = 0

fn C.LoadLibrary(libfilename &u16) voidptr

fn C.GetProcAddress(handle voidptr, procname &u8) voidptr

fn C.FreeLibrary(handle voidptr) bool

type FN_vinit_caller = fn ()

type FN_vcleanup_caller = fn ()

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
	return C.GetProcAddress(handle, voidptr(symbol.str))
}

// dlerror provides a text error diagnostic message for functions in `dl`
// it returns a human-readable string, describing the most recent error
// that occurred from a call to one of the `dl` functions, since the last
// call to dlerror()
pub fn dlerror() string {
	// https://docs.microsoft.com/en-us/windows/win32/api/errhandlingapi/nf-errhandlingapi-getlasterror
	// Unlike dlerror(), GetLastError returns just an error code, that is function specific.
	cerr := int(C.GetLastError())
	return 'error code ${cerr}'
}
