// vtest build: linux && !sanitized_job?
// vtest vflags: -gc none -d track_heap
// Count allocation events directly: Linux RSS accounting is batched, and reading
// /proc/self/statm would allocate in the measurement itself.
import crypto.hmac
import crypto.sha1
import crypto.sha256
import crypto.sha512

#flag @VEXEROOT/vlib/crypto/hmac/testdata/alloc_counter.o

@[c_extern]
fn C.hmac_test_allocation_count() u64

const messages = 50_000

fn allocation_growth[D](h fn () D) u64 {
	mut mac := hmac.new_hmac(h, 'key'.bytes())
	mut out := []u8{len: mac.size()}
	message := []u8{len: 100, init: u8(index)}
	// warm up, so that one-time allocations are not counted
	mac.write(message) or { panic(err) }
	mac.sum_into(mut out)
	before := C.hmac_test_allocation_count()
	for _ in 0 .. messages {
		mac.reset()
		mac.write(message) or { panic(err) }
		mac.sum_into(mut out)
	}
	return C.hmac_test_allocation_count() - before
}

fn test_allocation_counter_observes_a_real_allocation() {
	before := C.hmac_test_allocation_count()
	// Own this probe allocation and release it after verifying the hook.
	allocation := unsafe { malloc(1) }
	assert C.hmac_test_allocation_count() > before
	unsafe { free(allocation) }
}

fn test_reset_write_and_sum_into_do_not_allocate() {
	sha1_allocations := allocation_growth(sha1.new)
	assert sha1_allocations == 0, 'sha1: ${sha1_allocations} allocations'
	sha256_allocations := allocation_growth(sha256.new)
	assert sha256_allocations == 0, 'sha256: ${sha256_allocations} allocations'
	sha512_allocations := allocation_growth(sha512.new)
	assert sha512_allocations == 0, 'sha512: ${sha512_allocations} allocations'
}
