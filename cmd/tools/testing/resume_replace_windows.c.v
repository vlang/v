module testing

import time

// A replacement refused for a transient reason (see replace_record) is retried this many
// times, this far apart.
const replace_retry_attempts = 20
const replace_retry_delay = 25 * time.millisecond

fn C.MoveFileExW(lp_existing_file_name &u16, lp_new_file_name &u16, flags u32) int
fn C.GetLastError() u32

// replace_record moves `src` onto `dst`, replacing `dst` if it exists. On Windows a
// single MoveFileExW call does the replacement, and when it fails the previous `dst`
// is left in place. os.mv cannot do that there: it deletes `dst` before copying.
// A replacement refused for a transient reason, such as a reader holding `dst` open, is
// retried for about half a second before the error is returned.
fn replace_record(src string, dst string) ! {
	// Convert separators as os.rename does. to_wide allocates, so the buffers are
	// released by hand once the call is done.
	src_wide := src.replace('/', '\\').to_wide()
	dst_wide := dst.replace('/', '\\').to_wide()
	defer {
		unsafe {
			free(src_wide)
			free(dst_wide)
		}
	}
	// MOVEFILE_REPLACE_EXISTING is 0x1. A reader that holds the record open for a moment
	// makes the call fail with access denied or a sharing violation, so those failures
	// are retried for a short while. The error code is read right after the failed
	// call, before anything else can change it.
	mut attempt := 1
	for C.MoveFileExW(src_wide, dst_wide, u32(0x1)) == 0 {
		code := C.GetLastError()
		if attempt >= replace_retry_attempts || code !in [u32(5), u32(32), u32(33)] {
			return error('failed to replace `${dst}` (Windows error ${code})')
		}
		attempt++
		time.sleep(replace_retry_delay)
	}
}
