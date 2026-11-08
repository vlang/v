module driver

$if macos {
	#include <sys/clonefile.h>
}

fn C.clonefile(src &char, dst &char, flags u32) int

// v3_clone_file makes `destination` a copy of `source` that shares its blocks,
// where the file system can (APFS on macOS). It reports false when it cannot,
// and the caller copies the file instead.
fn v3_clone_file(source string, destination string) bool {
	$if macos {
		return C.clonefile(&char(source.str), &char(destination.str), 0) == 0
	} $else {
		return false
	}
}
