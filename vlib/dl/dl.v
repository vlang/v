module dl

pub const (
	version = 1
	dl_ext  = get_shared_library_extension()
)

// get_shared_library_extension returns the platform dependent shared library extension
// i.e. .dll on windows, .so on most unixes, .dylib on macos.
[inline]
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
[inline]
pub fn get_libname(libname string) string {
	return '$libname$dl.dl_ext'
}
