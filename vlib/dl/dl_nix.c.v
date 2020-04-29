module dl

#include <dlfcn.h>

fn C.dlopen(filename charptr, flags int) voidptr
fn C.dlsym(handle voidptr, symbol charptr) voidptr
fn C.dlclose(handle voidptr) int

pub const (
	RTLD_NOW = C.RTLD_NOW
	DL_EXT = '.so'
)

pub fn open(filename string, flags int) voidptr {
	return C.dlopen(filename.str, flags)
}

pub fn close(handle voidptr) bool {
	return C.dlclose(handle) == 0
}

pub fn sym(handle voidptr, symbol string) voidptr {
	return C.dlsym(handle, symbol.str)
}
