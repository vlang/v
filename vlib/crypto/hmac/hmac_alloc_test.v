// vtest build: linux && !sanitized_job?
// vtest vflags: -gc none
// Without a GC every heap allocation is leaked, so the growth of RSS shows
// whether `Hmac.reset`, `Hmac.write` and `Hmac.sum_into` allocate.
import crypto.hmac
import crypto.sha1
import crypto.sha256
import crypto.sha512
import os

const messages = 50_000
const max_growth = 64 * 1024

fn rss_bytes() i64 {
	statm := os.read_file('/proc/self/statm') or { panic(err) }
	return statm.split(' ')[1].i64() * os.page_size()
}

fn rss_growth[D](h fn () D) i64 {
	mut mac := hmac.new_hmac(h, 'key'.bytes())
	mut out := []u8{len: mac.size()}
	message := []u8{len: 100, init: u8(index)}
	// warm up, so that one-time allocations are not counted
	mac.write(message) or { panic(err) }
	mac.sum_into(mut out)
	before := rss_bytes()
	for _ in 0 .. messages {
		mac.reset()
		mac.write(message) or { panic(err) }
		mac.sum_into(mut out)
	}
	return rss_bytes() - before
}

fn test_reset_write_and_sum_into_do_not_allocate() {
	sha1_growth := rss_growth(sha1.new)
	assert sha1_growth < max_growth, 'sha1: RSS grew by ${sha1_growth} bytes'
	sha256_growth := rss_growth(sha256.new)
	assert sha256_growth < max_growth, 'sha256: RSS grew by ${sha256_growth} bytes'
	sha512_growth := rss_growth(sha512.new)
	assert sha512_growth < max_growth, 'sha512: RSS grew by ${sha512_growth} bytes'
}
