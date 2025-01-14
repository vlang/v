import rand
import crypto.ecdsa

// The test file placed on its own directory. Its for workaround for
// module lookup problem, because there are two rand module availables,
// between `crypto.rand` and `rand` module.
// See [the talk](https://discord.com/channels/592103645835821068/592294828432424960/1328198034806407311) on discord.
fn test_new_key_from_seed_with_random_size_and_data() ! {
	num_iters := 100
	// default prime256v1 curve key size was 32 bytes.
	max_key_size := i32(48)
	for i := 0; i <= num_iters; i++ {
		m := rand.i32n(max_key_size)!
		random_bytes := rand.bytes(m)!
		pvkey := ecdsa.new_key_from_seed(random_bytes) or {
			// With default size, would error on m > 32 or m == 0
			// dump(m)
			if m == 0 {
				assert err == error('Seed with null-length was not allowed')
			} else if m > 32 {
				assert err == error('Seed length exceeds key size')
			} else {
				assert err == error('EC_KEY_check_key failed')
			}
			continue
		}
		ret_seed := pvkey.seed()!
		// this test would fail if random_bytes was leading by zeros bytes.
		assert random_bytes == ret_seed
		pvkey.free()
	}
}
