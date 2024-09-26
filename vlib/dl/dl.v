module dl

pub const version = 1

pub const dl_ext = get_shared_library_extension()

pub const rtld_next = voidptr(-1)

// get_shared_library_extension returns the platform dependent shared library extension
// i.e. .dll on windows, .so on most unixes, .dylib on macos.
@[inline]
pub fn get_shared_library_extension() string {
	return $if windows {
		'.dll'
	} $else $if macos {
		'.dylib'
	} $else {
		'.so'
	}
}

// get_libname returns a library name with the operating system specific extension for
// shared libraries.
@[inline]
pub fn get_libname(libname string) string {
	return '${libname}${dl_ext}'
}

// open_opt tries to load a given dynamic shared object.
pub fn open_opt(filename string, flags int) !voidptr {
	shared_object_handle := open(filename, flags)
	if shared_object_handle == 0 {
		e := dlerror()
		return error(e)
	}
	return shared_object_handle
}

// sym_opt returns the address of a symbol in a given shared object, if found.
// Unlike sym, sym_opt returns an option.
pub fn sym_opt(shared_object_handle voidptr, symbol string) !voidptr {
	sym_handle := sym(shared_object_handle, symbol)
	if sym_handle == 0 {
		e := dlerror()
		return error(e)
	}
	return sym_handle
}
