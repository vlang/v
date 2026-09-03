module fastc

#include <CommonCrypto/CommonDigest.h>

fn C.CC_SHA256(data voidptr, len u32, md &u8) &u8

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
	worker_count := 4
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
