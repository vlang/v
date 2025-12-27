import hash

fn test_wyhash_basic() {
	// Official wyhash final v4.2 test vectors (seed=0, default secret)
	assert hash.sum64_string('', 0).hex_full() == '93228a4de0eec5a2'
	assert hash.sum64_string('a', 1).hex_full() == 'c5bac3db178713c4'
	assert hash.sum64_string('abc', 2).hex_full() == 'a97f2f7b1d9b3314'
	assert hash.sum64_string('message digest', 3).hex_full() == '786d1f1df3801df4'
	assert hash.sum64_string('abcdefghijklmnopqrstuvwxyz', 4).hex_full() == 'dca5a8138ad37c87'
	assert hash.sum64_string('ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789',
		5).hex_full() == 'b9e734f117cfaf70'
	assert hash.sum64_string('12345678901234567890123456789012345678901234567890123456789012345678901234567890',
		6).hex_full() == '6cc5eab49a92d617'
	assert hash.wyhash64_c(u64(1234567890), u64(7777777777)) == 13699604260906621654
	assert hash.wymum(u64(1234567890), u64(7777777777)) == 9602194699039780530
}

fn test_wyhash_byte_array() {
	data := 'hello world'.bytes()
	assert hash.sum64(data, 0) == hash.sum64_string('hello world', 0)

	empty := []u8{}
	assert hash.sum64(empty, 0) == hash.sum64_string('', 0)
}

fn test_wyhash64() {
	// wyhash64(A, B) = wymix(A ^ secret[0], B ^ secret[1])
	// Using default secrets: secret[0]=0x2d358dccaa6c78a5, secret[1]=0x8bb84b93962eacc9
	a := u64(1234567890)
	b := u64(7777777777)
	expected := u64(13699604260906621654)
	assert hash.wyhash64_c(a, b) == expected
}

fn test_wyrand_prng() {
	mut seed := u64(12345)

	r1 := hash.wyrand(&seed)
	r2 := hash.wyrand(&seed)
	r3 := hash.wyrand(&seed)

	assert r1.hex_full() == 'b699757c9eced02d'
	assert r2.hex_full() == 'fb4802058acc1d22'
	assert r3.hex_full() == 'cb97a000ea275afb'

	// Ensure it's deterministic
	mut seed2 := u64(12345)
	assert hash.wyrand(&seed2) == r1
	assert hash.wyrand(&seed2) == r2
}

fn test_wy2u01() {
	r_zero := u64(0)
	assert hash.wy2u01(r_zero) == 0.0

	r_max := u64(0xFFFFFFFFFFFFFFFF)
	assert hash.wy2u01(r_max) >= 0.9999999999999996 // close to 1.0, never reaches 1.0
}

fn test_map_hash_consistency() {
	// Ensure map string hashing matches public sum64_string (seed 0, default secret)
	s := 'map key example'
	map_style := hash.wyhash_c(s.str, u64(s.len), 0) // this is what map uses internally
	public_style := hash.sum64_string(s, 0)
	assert map_style == public_style
}
