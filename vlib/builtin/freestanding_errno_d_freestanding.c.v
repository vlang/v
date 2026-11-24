module builtin

$if (linux || (macos && amd64)) && !native && !i386 {
	// Native C errno has its own canonical C TLS cell. BF errors use Linux
	// numbers, but their state must remain independent of POSIX callers.
	__global bf_native_errno_cell i32
}
