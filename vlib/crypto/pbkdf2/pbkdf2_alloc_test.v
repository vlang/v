// vtest build: linux && !sanitized_job?
// vtest vflags: -gc none
// Without a GC every heap allocation is leaked, so the growth of RSS shows how
// much `pbkdf2.key` allocates. It used to allocate in every iteration, which
// leaked ~6.6 MB per derivation with c = 4096.
import crypto.pbkdf2
import crypto.sha1
import crypto.sha256
import crypto.sha512
import hash
import os

const calls = 20
const max_growth_per_call = 64 * 1024

fn rss_bytes() i64 {
	statm := os.read_file('/proc/self/statm') or { panic(err) }
	return statm.split(' ')[1].i64() * os.page_size()
}

fn rss_growth_per_call(h hash.Hash) i64 {
	password := 'pencil'.bytes()
	salt := 'QSXCR+Q6sek8bf92'.bytes()
	// warm up, so that one-time allocations are not counted
	_ := pbkdf2.key(password, salt, 4096, 32, h) or { panic(err) }
	before := rss_bytes()
	for _ in 0 .. calls {
		_ := pbkdf2.key(password, salt, 4096, 32, h) or { panic(err) }
	}
	return (rss_bytes() - before) / calls
}

fn test_sha1_does_not_allocate_per_iteration() {
	growth := rss_growth_per_call(sha1.new())
	assert growth < max_growth_per_call, 'RSS grew by ${growth} bytes per call'
}

fn test_sha256_does_not_allocate_per_iteration() {
	growth := rss_growth_per_call(sha256.new())
	assert growth < max_growth_per_call, 'RSS grew by ${growth} bytes per call'
}

fn test_sha512_does_not_allocate_per_iteration() {
	growth := rss_growth_per_call(sha512.new())
	assert growth < max_growth_per_call, 'RSS grew by ${growth} bytes per call'
}
