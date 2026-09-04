module fastc

import os

#include <CommonCrypto/CommonDigest.h>

#include <sys/mman.h>

fn C.CC_SHA256(data voidptr, len u32, md &u8) &u8

// fastc_sign_macho_adhoc_mapped extends and patches the linker output through
// a private mapping. The page hashes can read that mapping directly, avoiding a
// four-megabyte read and a second copy back through write syscalls per self-host.
fn fastc_sign_macho_adhoc_mapped(path string) ! {
	original_len := int(os.file_size(path))
	capacity := fastc_macho_sign_capacity(original_len, path.all_after_last('/'))
	mut out := os.open_file(path, 'r+')!
	if C.ftruncate(i32(out.fd), u64(capacity)) != 0 {
		out.close()
		return error('`${path}`: could not extend the Mach-O file for signing')
	}
	mapped := unsafe {
		C.mmap(nil, usize(capacity), C.PROT_READ | C.PROT_WRITE, C.MAP_PRIVATE, out.fd, 0)
	}
	if mapped == voidptr(-1) {
		_ = C.ftruncate(i32(out.fd), u64(original_len))
		out.close()
		return error('`${path}`: could not map the Mach-O file for signing')
	}
	mut file := unsafe { mapped.vbytes(capacity) }
	patch := fastc_patch_macho_signature(mut file, original_len, path) or {
		unsafe { C.munmap(mapped, usize(capacity)) }
		_ = C.ftruncate(i32(out.fd), u64(original_len))
		out.close()
		return err
	}
	load_commands := file[..patch.load_end].clone()
	tail := file[patch.tail_start..patch.final_len].clone()
	if unsafe { C.munmap(mapped, usize(capacity)) } != 0 {
		out.close()
		return error('`${path}`: could not release the mapped Mach-O image')
	}
	// Drop the old signature before publishing the new tail. Besides avoiding a
	// whole-file rewrite, this size transition invalidates macOS's cached signing
	// state so the executable can be launched immediately.
	if C.ftruncate(i32(out.fd), u64(patch.tail_start)) != 0 {
		out.close()
		return error('`${path}`: could not remove the old Mach-O signature')
	}
	out.write_to(0, load_commands) or {
		out.close()
		return err
	}
	out.write_to(u64(patch.tail_start), tail) or {
		out.close()
		return err
	}
	out.close()
}

// fastc_hash_code_pages_native writes the SHA-256 of the pages [0, page_count)
// of `data` (the last one ending at `code_limit`) to `hashes`, with the
// system's hardware-accelerated digest: a few megabytes per millisecond,
// where the V implementation compiled by TinyCC manages tens per second.
// Larger files are split over a few threads (the hashing allocates nothing).
fn fastc_hash_code_pages_native(data &u8, hashes &u8, page_count int, code_limit int) {
	if page_count < 256 {
		fastc_hash_native_range(data, hashes, 0, page_count, code_limit)
		return
	}
	available_workers := fastc_nr_cpus()
	worker_count := if available_workers < 16 { available_workers } else { 16 }
	mut workers := [
		spawn fastc_hash_native_range(data, hashes, 0, page_count / worker_count, code_limit),
	]
	for worker in 1 .. worker_count {
		workers << spawn fastc_hash_native_range(data, hashes, page_count * worker / worker_count, page_count * (worker + 1) / worker_count, code_limit)
	}
	for worker in workers {
		worker.wait()
	}
}

fn fastc_hash_native_range(data &u8, hashes &u8, page_start int, page_end int, code_limit int) bool {
	for page in page_start .. page_end {
		start := page * cs_page_size
		mut end := start + cs_page_size
		if end > code_limit {
			end = code_limit
		}
		unsafe {
			C.CC_SHA256(data + start, u32(end - start), hashes + page * cs_hash_size)
		}
	}
	return true
}
