// vtest build: linux && !sanitized_job?
// vtest vflags: -gc none
// Without a GC every heap allocation is leaked, so the growth of RSS shows how
// much `expand` allocates. It used to re-key HMAC and allocate for every output
// block, which leaked ~670 KB for a maximum length (255 block) SHA-256 output.
import crypto.hkdf
import crypto.sha1
import crypto.sha256
import crypto.sha512
import hash
import os

const calls = 20

fn rss_bytes() i64 {
	statm := os.read_file('/proc/self/statm') or { panic(err) }
	return statm.split(' ')[1].i64() * os.page_size()
}

fn check_rss_growth(name string, h fn () hash.Hash, size int) ! {
	prk := []u8{len: 32, init: u8(index)}
	key_length := 255 * size
	// the returned key itself is leaked too, so allow for it
	max_growth_per_call := key_length + 16 * 1024
	// warm up, so that one-time allocations are not counted
	_ := hkdf.expand(h, prk, 'info', key_length)!
	before := rss_bytes()
	for _ in 0 .. calls {
		_ := hkdf.expand(h, prk, 'info', key_length)!
	}
	growth := (rss_bytes() - before) / calls
	assert growth < max_growth_per_call, '${name}: RSS grew by ${growth} bytes per call'
}

fn test_expand_does_not_allocate_per_block() {
	check_rss_growth('sha1', fn () hash.Hash {
		return sha1.new()
	}, sha1.size)!
	check_rss_growth('sha256', fn () hash.Hash {
		return sha256.new()
	}, sha256.size)!
	check_rss_growth('sha512', fn () hash.Hash {
		return sha512.new()
	}, sha512.size)!
}
