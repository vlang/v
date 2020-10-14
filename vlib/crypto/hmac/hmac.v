// HMAC: Keyed-Hashing for Message Authentication  implemented in v
// implementation based on https://tools.ietf.org/html/rfc2104
module hmac

const (
	ipad = []byte{len: 256, init: 0x36} // TODO is 256 enough??
	opad = []byte{len: 256, init: 0x5C}
	npad = []byte{len: 256, init: 0}
)

// Returns an HMAC byte array, depending on the hash algorithm used
pub fn new(key, data []byte, hash_func fn (bytes []byte) []byte, blocksize int) []byte {
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
