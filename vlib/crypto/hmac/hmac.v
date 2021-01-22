// HMAC: Keyed-Hashing for Message Authentication  implemented in v
// implementation based on https://tools.ietf.org/html/rfc2104
module hmac

import crypto.internal.subtle

const (
	ipad = []byte{len: 256, init: 0x36} // TODO is 256 enough??
	opad = []byte{len: 256, init: 0x5C}
	npad = []byte{len: 256, init: 0}
)

// new returns a HMAC byte array, depending on the hash algorithm used.
pub fn new(key []byte, data []byte, hash_func fn (bytes []byte) []byte, blocksize int) []byte {
	mut b_key := []byte{}
	if key.len <= blocksize {
		b_key = key.clone() // TODO: remove .clone() once https://github.com/vlang/v/issues/6604 gets fixed
	} else {
		b_key = hash_func(key)
	}
	if b_key.len < blocksize {
		b_key << npad[..blocksize - b_key.len]
	}
	mut inner := []byte{}
	for i, b in ipad[..blocksize] {
		inner << b_key[i] ^ b
	}
	inner << data
	inner_hash := hash_func(inner)
	mut outer := []byte{cap: b_key.len}
	for i, b in opad[..blocksize] {
		outer << b_key[i] ^ b
	}
	outer << inner_hash
	digest := hash_func(outer)
	return digest
}

// equal compares 2 MACs for equality, without leaking timing info.
// NB: if the lengths of the 2 MACs are different, probably a completely different
// hash function was used to generate them => no useful timing information.
pub fn equal(mac1 []byte, mac2 []byte) bool {
	return subtle.constant_time_compare(mac1, mac2) == 1
}
