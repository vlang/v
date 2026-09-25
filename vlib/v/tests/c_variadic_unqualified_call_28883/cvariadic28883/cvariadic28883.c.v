module cvariadic28883

#include <stdarg.h>
#include <stdio.h>

@[typedef]
struct C.va_list {}

fn C.va_start(voidptr, voidptr)
fn C.va_end(voidptr)
fn C.vsnprintf(buf &char, size usize, fmt &char, ap C.va_list) int

// cvariadic28883 shares its module's name, like the fn in the issue, and
// forwards its C-style variadic args to vsnprintf.
pub fn cvariadic28883(buf &char, fmt &char, ...) int {
	ap := C.va_list{}
	C.va_start(ap, fmt)
	n := C.vsnprintf(buf, 64, fmt, ap)
	C.va_end(ap)
	return n
}

// format forwards its C-style variadic args to vsnprintf.
pub fn format(buf &char, fmt &char, ...) int {
	ap := C.va_list{}
	C.va_start(ap, fmt)
	n := C.vsnprintf(buf, 64, fmt, ap)
	C.va_end(ap)
	return n
}

// same_name_plain calls `cvariadic28883` unqualified, without variadic args.
pub fn same_name_plain(buf &char) int {
	return cvariadic28883(buf, c'plain')
}

// same_name_args calls `cvariadic28883` unqualified, with variadic args.
pub fn same_name_args(buf &char) int {
	return cvariadic28883(buf, c'%d=%d', 4, 7)
}

// format_plain calls `format` unqualified, without variadic args.
pub fn format_plain(buf &char) int {
	return format(buf, c'plain')
}

// format_args calls `format` unqualified, with variadic args.
pub fn format_args(buf &char) int {
	return format(buf, c'%d-%d-%d', 1, 42, 3)
}
