module dl

#include <dlfcn.h>

$if linux {
	#flag -ldl
}

pub const rtld_now = C.RTLD_NOW
pub const rtld_lazy = C.RTLD_LAZY
pub const rtld_global = C.RTLD_GLOBAL
pub const rtld_local = C.RTLD_LOCAL
pub const rtld_nodelete = C.RTLD_NODELETE
pub const rtld_noload = C.RTLD_NOLOAD

fn C.dlopen(filename &char, flags int) voidptr

fn C.dlsym(handle voidptr, symbol &char) voidptr

fn C.dlclose(handle voidptr) int

fn C.dlerror() &char

// open loads a given dynamic shared object.
pub fn open(filename string, flags int) voidptr {
	return C.dlopen(&char(filename.str), flags)
}

// close frees a given shared object.
pub fn close(handle voidptr) bool {
	return C.dlclose(handle) == 0
}

// sym returns an address of a symbol in a given shared object.
pub fn sym(handle voidptr, symbol string) voidptr {
	return C.dlsym(handle, &char(symbol.str))
}

// dlerror provides a text error diagnostic message for functions in `dl`.
// It returns a human-readable string, describing the most recent error
// that occurred from a call to one of the `dl` functions, since the last
// call to dlerror().
pub fn dlerror() string {
	sptr := C.dlerror()
	if sptr == unsafe { nil } {
		return ''
	}
	return unsafe { cstring_to_vstring(sptr) }
}
