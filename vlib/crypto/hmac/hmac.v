// HMAC: Keyed-Hashing for Message Authentication  implemented in v
// implementation based on https://tools.ietf.org/html/rfc2104
module hmac

import crypto.internal.subtle

// new returns a HMAC byte array, depending on the hash algorithm used.
pub fn new(key []u8, data []u8, hash_func fn ([]u8) []u8, blocksize int) []u8 {
	mut inner := []u8{len: blocksize, init: 0x36}
	mut outer := []u8{len: blocksize, init: 0x5C}

	mut b_key := []u8{}
	if key.len <= blocksize {
		b_key = key.clone() // TODO: remove .clone() once https://github.com/vlang/v/issues/6604 gets fixed
	} else {
		b_key = hash_func(key)
	}
	if b_key.len > blocksize {
		b_key = b_key[..blocksize].clone()
	}
	for i, b in b_key {
		inner[i] = b ^ 0x36
		outer[i] = b ^ 0x5c
	}
	inner << data
	inner_hash := hash_func(inner)
	outer << inner_hash
	digest := hash_func(outer)
	return digest
}

// equal compares 2 MACs for equality, without leaking timing info.
// Note: if the lengths of the 2 MACs are different, probably a completely different
// hash function was used to generate them => no useful timing information.
pub fn equal(mac1 []u8, mac2 []u8) bool {
	return subtle.constant_time_compare(mac1, mac2) == 1
}
