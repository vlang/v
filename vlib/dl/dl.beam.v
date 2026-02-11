// BEAM backend stub for dl module
// Not applicable on BEAM: The dl module provides dynamic loading of C shared
// libraries (dlopen/dlsym/dlclose). BEAM does not support loading C shared
// libraries at runtime in the same way — instead, it uses NIFs (Native
// Implemented Functions) compiled and linked at build time, or Port drivers.
// These stubs return safe defaults (nil/true/error string) to allow code
// that references dl to compile on BEAM.
module dl

pub const rtld_now = 0x0002
pub const rtld_lazy = 0x0001
pub const rtld_global = 0x0100
pub const rtld_local = 0x0000
pub const rtld_nodelete = 0x1000
pub const rtld_noload = 0x0004

// open loads a given dynamic shared object.
// Not applicable on BEAM: C dynamic loading is not supported.
// Returns nil (no handle).
pub fn open(filename string, flags int) voidptr {
	return unsafe { nil }
}

// close frees a given shared object.
// Not applicable on BEAM: no-op since open never returns a real handle.
// Returns true (success) for API compatibility.
pub fn close(handle voidptr) bool {
	return true
}

// sym returns an address of a symbol in a given shared object.
// Not applicable on BEAM: C symbol lookup is not supported.
// Returns nil (no symbol found).
pub fn sym(handle voidptr, symbol string) voidptr {
	return unsafe { nil }
}

// dlerror provides a text error diagnostic message.
// Not applicable on BEAM: always returns an error message explaining
// that dynamic C library loading is not available on the BEAM runtime.
pub fn dlerror() string {
	return 'dynamic loading not available on BEAM — use NIFs or Port drivers instead'
}
