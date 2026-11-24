module builtin

$if linux && !native && !i386 {
	// Native C errno has its own canonical C TLS cell. BF errors use Linux
	// numbers too, but their state must remain independent of POSIX callers.
	__global bf_native_errno_cell i32
}
