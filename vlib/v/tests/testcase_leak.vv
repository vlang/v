// This program is supposed to test the leak detector which
// is integrated with `-gc boehm_leak` at compile time.
//
// If compiled with `-gc boehm` or without `-gc` nothing
// will be reported.

fn main() {
	mut y := unsafe { malloc(1000) }
	// unsafe { free(y) } // leak if commented out
	y = voidptr(0)
	gc_check_leaks()
}
