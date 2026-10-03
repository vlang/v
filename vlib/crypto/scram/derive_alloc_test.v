// vtest build: linux && !sanitized_job?
// vtest vflags: -gc none
// Without a GC every heap allocation is leaked, so the growth of RSS shows how
// much `derive_credentials` allocates. `Hi()` used to allocate in every
// iteration, which leaked ~6 to 8 MB per derivation with 4096 iterations.
import crypto.scram
import os

const calls = 10
const max_growth_per_call = 64 * 1024

fn rss_bytes() i64 {
	statm := os.read_file('/proc/self/statm') or { panic(err) }
	return statm.split(' ')[1].i64() * os.page_size()
}

fn test_derive_credentials_does_not_allocate_per_iteration() {
	salt := 'QSXCR+Q6sek8bf92'.bytes()
	for mechanism in [scram.Mechanism.sha1, .sha256, .sha512] {
		// warm up, so that one-time allocations are not counted
		_ := scram.derive_credentials(mechanism, 'pencil', salt, 4096)!
		before := rss_bytes()
		for _ in 0 .. calls {
			_ := scram.derive_credentials(mechanism, 'pencil', salt, 4096)!
		}
		growth := (rss_bytes() - before) / calls
		assert growth < max_growth_per_call, '${mechanism.name()}: RSS grew by ${growth} bytes per call'
	}
}
