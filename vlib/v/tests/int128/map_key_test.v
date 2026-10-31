// A 128-bit key is 16 bytes, and the callbacks a map is built with have to read
// all of them. The 4-byte ones kept the low half, so two keys that shared it
// overwrote each other and both lookups returned the later value.
fn high_bit_key() u128 {
	return (u128(1) << 64) + u128(1)
}

fn test_distinct_wide_keys_do_not_overwrite_each_other() {
	mut m := map[u128]int{}
	m[u128(1)] = 10
	m[high_bit_key()] = 20
	assert m.len == 2
	assert m[u128(1)] == 10
	assert m[high_bit_key()] == 20
}

fn test_a_wide_key_that_differs_above_64_bits_stays_distinct() {
	mut m := map[u128]string{}
	for high in [u128(0), u128(1), u128(1) << 64, (u128(1) << 127)] {
		key := high + u128(7)
		m[key] = high.str()
	}
	assert m.len == 4
	assert m[u128(7)] == '0'
	assert m[u128(8)] == '1'
	assert m[(u128(1) << 64) + u128(7)] == '18446744073709551616'
}

fn test_a_signed_wide_key_keeps_its_sign() {
	mut m := map[i128]string{}
	m[i128(-1)] = 'minus one'
	m[i128(1)] = 'one'
	m[i128(-1) << 100] = 'far below zero'
	assert m.len == 3
	assert m[i128(-1)] == 'minus one'
	assert m[i128(1)] == 'one'
	assert m[i128(-1) << 100] == 'far below zero'
}

fn test_a_wide_key_survives_the_copy_into_the_map() {
	// The clone callback is the one that copies the key into the map, so a half
	// copy left the top half of every key at zero.
	key := (u128(1) << 100) + u128(9)
	mut m := map[u128]int{}
	m[key] = 1
	assert key in m
	assert m[key] == 1
	// This key shares its low half with the one above.
	other := (u128(1) << 100) + u128(10)
	m[other] = 2
	assert m.len == 2
	assert m[key] == 1
	assert m[other] == 2
}

fn test_a_wide_key_can_be_removed() {
	key := high_bit_key()
	mut m := map[u128]int{}
	m[key] = 5
	m.delete(key)
	assert m.len == 0
	assert key !in m
}

fn test_a_wide_map_value_is_not_a_key_question() {
	// The callbacks are chosen from the key type, so a wide value under a narrow
	// key still has to work.
	mut m := map[u64]u128{}
	m[u64(1)] = u128(1) << 100
	key := u64(0xffffffffffffffff)
	m[key] = u128(5)
	assert m.len == 2
	assert m[u64(1)] == u128(1) << 100
	assert m[key] == u128(5)
}
