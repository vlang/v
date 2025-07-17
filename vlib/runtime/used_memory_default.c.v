module runtime

// used_memory retrieves the current physical memory usage of the process.
// Note: implementation available only on FreeBSD, macOS, Linux, OpenBSD and
// Windows. Otherwise, returns 'used_memory: not implemented'.
pub fn used_memory() !u64 {
	return error('used_memory: not implemented')
}
