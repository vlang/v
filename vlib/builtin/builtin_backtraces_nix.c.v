module builtin

// <execinfo.h>
fn C.backtrace(a &voidptr, size i32) i32
fn C.backtrace_symbols(a &voidptr, size i32) &&char
fn C.backtrace_symbols_fd(a &voidptr, size i32, fd i32)

$if macos {
	#include <mach-o/dyld.h>

	fn C._dyld_get_image_header(image_index u32) voidptr
}
