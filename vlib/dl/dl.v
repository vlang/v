module dl

pub const (
	version = 1
	dl_ext  = get_shared_library_extension()
)

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
