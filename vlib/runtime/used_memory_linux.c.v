module runtime

import os

// used_memory retrieves the current physical memory usage of the process.
@[manualfree]
pub fn used_memory() !u64 {
	buffer := [1024]u8{}
	pc := unsafe { &buffer[0] }
	// see https://man7.org/linux/man-pages/man5/proc_pid_stat.5.html for a detailed description of the format.
	// Here is an example:
	// 699989 (test_program) R 178038 699989 178038 34816 699989 0 102 0 0 0 0 0 0 0 20 0 1 0 84188763 5726208 129 18446744073709551615 93824992239616 93824992257073 140737488345056 0 0 0 0 0 0 0 0 0 17 0 0 0 0 0 0 93824992275088 93824992276672 93824992280576 140737488346201 140737488346221 140737488346221 140737488351211 0
	// => read the file in the fixed buffer, search to after the `)` to skip the variable name part, then scan till the `129` RSS value, and then convert it from pages to bytes.
	mut f := os.open_file('/proc/self/stat', 'r')!
	defer { f.close() }
	read := f.read_into_ptr(pc, buffer.len)!
	if read <= 0 {
		return error('could not read from /proc/self/stat')
	}
	mut c := 0
	unsafe {
		// scan to after the process name (`comm`) field:
		for c < buffer.len && *pc != `)` {
			pc++
			c++
		}
		mut spaces := 0
		// the following fields, are space separated numbers, skip past them to the `rss` field:
		for c < buffer.len && spaces < 22 {
			if *pc == ` ` {
				spaces++
			}
			pc++
			c++
		}
		mut ndigits := 0
		// scan till the end of the `rss` field:
		for c < buffer.len && *(pc + ndigits) != ` ` {
			ndigits++
			c++
		}
		rss_pages := pc.vstring_with_len(ndigits).u64()
		page_size := C.sysconf(C._SC_PAGESIZE)
		return u64(rss_pages * page_size)
	}
}
