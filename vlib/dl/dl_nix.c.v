module dl

#include <dlfcn.h>

pub const (
	rtld_now  = C.RTLD_NOW
	rtld_lazy = C.RTLD_LAZY
)

fn C.dlopen(filename charptr, flags int) voidptr

fn C.dlsym(handle voidptr, symbol charptr) voidptr

fn C.dlclose(handle voidptr) int

fn C.dlerror() charptr

// open loads the dynamic shared object.
pub fn open(filename string, flags int) voidptr {
	return C.dlopen(charptr(filename.str), flags)
}

// close frees a given shared object.
pub fn close(handle voidptr) bool {
	return C.dlclose(handle) == 0
}

// sym returns an address of a symbol in a given shared object.
pub fn sym(handle voidptr, symbol string) voidptr {
	return C.dlsym(handle, charptr(symbol.str))
}

// dlerror provides a text error diagnostic message for functions in `dl`
// it returns a human-readable string, describing the most recent error 
// that occurred from a call to one of the `dl` functions, since the last
// call to dlerror()
pub fn dlerror() string {
	return unsafe { cstring_to_vstring(byteptr(C.dlerror())) }
}
