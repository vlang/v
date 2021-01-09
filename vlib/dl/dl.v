module dl

pub const (
	version = 1
	dl_ext  = get_shared_library_extension()
)

// get_shared_library_extension returns the platform dependent shared library extension
// i.e. .dll on windows, .so on most unixes, .dylib on macos.
pub fn get_shared_library_extension() string {
	mut res := '.so'
	$if macos {
		res = '.dylib'
	}
	$if windows {
		res = '.dll'
	}
	return res
}
