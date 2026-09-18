module arm64

#include <CommonCrypto/CommonDigest.h>

fn C.CC_SHA256(const_data voidptr, len u32, md &u8) &u8

// sha256_hash_pages_native hashes code-signature pages with macOS's
// hardware-accelerated SHA-256 implementation.
fn sha256_hash_pages_native(data &u8, mut hashes []u8, page_count int, code_limit int) {
	if page_count < 256 {
		sha256_hash_native_range(data, hashes.data, 0, page_count, code_limit)
		return
	}
	worker_count := 8
	mut workers := []thread bool{cap: worker_count}
	for worker in 0 .. worker_count {
		workers << spawn sha256_hash_native_range(data, hashes.data, page_count * worker / worker_count, page_count * (worker + 1) / worker_count, code_limit)
	}
	workers.wait()
}

fn sha256_hash_native_range(data &u8, hashes voidptr, page_start int, page_end int, code_limit int) bool {
	for page in page_start .. page_end {
		start := page * cs_page_size_arm64
		mut end := start + cs_page_size_arm64
		if end > code_limit {
			end = code_limit
		}
		unsafe {
			C.CC_SHA256(data + start, u32(end - start), &u8(hashes) + page * cs_hash_size)
		}
	}
	return true
}
