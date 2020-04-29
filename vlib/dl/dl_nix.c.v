module dl

#include <dlfcn.h>

fn C.dlopen(filename charptr, flags int) voidptr
fn C.dlsym(handle voidptr, symbol charptr) voidptr
fn C.dlclose(handle voidptr) int

pub const (
	RTLD_NOW = C.RTLD_NOW
	DL_EXT = '.so'
)

// open loads the dynamic shared object.
pub fn open(filename string, flags int) voidptr {
	return C.dlopen(filename.str, flags)
}

// close frees a given shared object.
pub fn close(handle voidptr) bool {
	return C.dlclose(handle) == 0
}

// sym returns an address of a symbol in a given shared object.
pub fn sym(handle voidptr, symbol string) voidptr {
	return C.dlsym(handle, symbol.str)
}
