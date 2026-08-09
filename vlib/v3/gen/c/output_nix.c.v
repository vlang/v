module c

import os

#include <sys/mman.h>
#include <unistd.h>

fn write_c_output_mapped(path string, prefix []u8, segments []string, tail string) ! {
	mut total := prefix.len + tail.len
	for segment in segments {
		total += segment.len
	}
	mut file := os.open_file(path, 'w+b')!
	if total == 0 {
		file.close()
		return
	}
	if C.ftruncate(file.fd, u64(total)) != 0 {
		file.close()
		return error('ftruncate failed (errno ${C.errno})')
	}
	mapped := unsafe {
		C.mmap(nil, usize(total), C.PROT_READ | C.PROT_WRITE, C.MAP_SHARED, file.fd, 0)
	}
	if mapped == C.MAP_FAILED {
		file.close()
		return error('mmap failed (errno ${C.errno})')
	}
	mut offset := 0
	unsafe {
		if prefix.len > 0 {
			vmemcpy(&u8(mapped) + offset, prefix.data, prefix.len)
			offset += prefix.len
		}
		for segment in segments {
			if segment.len > 0 {
				vmemcpy(&u8(mapped) + offset, segment.str, segment.len)
				offset += segment.len
			}
		}
		if tail.len > 0 {
			vmemcpy(&u8(mapped) + offset, tail.str, tail.len)
		}
	}
	// Keep the shared mapping alive for the short remaining lifetime of the
	// compiler process. The generated file is coherent as soon as the stores
	// above complete; unmapping it here can synchronously reclaim thousands of
	// dirty VM pages before the C compiler has even opened the file.
	file.close()
}
