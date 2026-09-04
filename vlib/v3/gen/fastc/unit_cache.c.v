module fastc

import os

fn C.wyhash(&u8, u64, u64, &u64) u64

@[c_extern]
fn C.clonefile(&char, &char, int) int

@[inline]
fn fastc_unit_cache_hash(text string, seed u64) u64 {
	return C.wyhash(text.str, u64(text.len), seed, &u64(voidptr(C._wyp)))
}

fn fastc_copy_cached_link(source string, destination string) ! {
	$if macos {
		// clonefile creates a copy-on-write executable, avoiding both the cost of
		// copying a self-host and the cache corruption risk of a hard link.
		os.rm(destination) or {}
		if C.clonefile(source.str, destination.str, 0) == 0 {
			return
		}
	}
	os.cp(source, destination)!
}
