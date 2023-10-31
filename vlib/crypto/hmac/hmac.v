// HMAC: Keyed-Hashing for Message Authentication  implemented in v
// implementation based on https://tools.ietf.org/html/rfc2104
module hmac

import crypto.internal.subtle

const (
	ipad = []u8{len: 256, init: 0x36} // TODO is 256 enough??
	opad = []u8{len: 256, init: 0x5C}
	npad = []u8{len: 256, init: 0}
)

// new returns a HMAC byte array, depending on the hash algorithm used.
pub fn new(key []u8, data []u8, hash_func fn ([]u8) []u8, blocksize int) []u8 {
	mut b_key := []u8{}
	if key.len <= blocksize {
		b_key = key.clone() // TODO: remove .clone() once https://github.com/vlang/v/issues/6604 gets fixed
	} else {
		b_key = hash_func(key)
	}
	if b_key.len < blocksize {
		b_key << hmac.npad[..blocksize - b_key.len]
	}
	mut inner := []u8{}
	for i, b in hmac.ipad[..blocksize] {
		inner << b_key[i] ^ b
	}
	inner << data
	inner_hash := hash_func(inner)
	mut outer := []u8{cap: b_key.len}
	for i, b in hmac.opad[..blocksize] {
		outer << b_key[i] ^ b
	}
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
