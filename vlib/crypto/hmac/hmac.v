// HMAC: Keyed-Hashing for Message Authentication  implemented in v
// implementation based on https://tools.ietf.org/html/rfc2104
module hmac

import crypto.sha1
import crypto.sha256
import crypto.sha512
import crypto.subtle

// new returns a HMAC byte array, depending on the hash algorithm used.
pub fn new(key []u8, data []u8, hash_func fn ([]u8) []u8, blocksize int) []u8 {
	mut inner := []u8{len: blocksize, init: 0x36}
	mut outer := []u8{len: blocksize, init: 0x5C}

	mut b_key := []u8{}
	if key.len <= blocksize {
		b_key =
			key.clone() // TODO: remove .clone() once https://github.com/vlang/v/issues/6604 gets fixed
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

// Hmac computes HMAC for one key and many messages. The key is processed once,
// in `new_hmac`, and after that `write`, `sum_into` and `reset` do not allocate.
// Each `sum_into` completes one message, and the next `write` starts a new one.
// `D` is the digest type of a `crypto.sha1`, `crypto.sha256` or `crypto.sha512`
// digest, for example `&sha256.Digest`. Use `hmac.new` for other hash functions.
//
// The keyed states are equivalent to the key, so treat an `Hmac` as a secret.
//
// Example:
// ```v
// import crypto.hmac
// import crypto.sha256
//
// mut mac := hmac.new_hmac(sha256.new, 'key'.bytes())
// mut out := []u8{len: mac.size()}
// for message in ['one', 'two'] {
// 	mac.write(message.bytes())!
// 	mac.sum_into(mut out)
// 	assert out == hmac.new('key'.bytes(), message.bytes(), sha256.sum256, sha256.block_size)
// }
// ```
pub struct Hmac[D] {
mut:
	// inner and outer are the states after absorbing `key ^ ipad` and `key ^ opad`
	inner D
	outer D
	// work holds the message written since the last `sum_into` or `reset`
	work D
}

// new_hmac returns an `Hmac` for `key`, using the digests returned by `h`, for
// example `sha256.new`, `sha256.new224`, `sha512.new384` or `sha1.new`. It is
// ready for `write`.
@[direct_array_access]
pub fn new_hmac[D](h fn () D, key []u8) &Hmac[D] {
	$if D !is &sha1.Digest && D !is &sha256.Digest && D !is &sha512.Digest {
		$compile_error('hmac.new_hmac: only crypto.sha1, crypto.sha256 and crypto.sha512 digests are supported, use hmac.new for other hash functions')
	}
	mut m := &Hmac[D]{
		inner: h()
		outer: h()
		work:  h()
	}
	// The key, padded with zeros to the block size (RFC 2104). Keys longer
	// than the block size are hashed first.
	block_size := m.work.block_size()
	mut pad := []u8{len: block_size}
	if key.len > block_size {
		m.work.write(key) or { panic(err) }
		m.work.checksum_into(mut pad)
	} else {
		copy(mut pad, key)
	}
	for i in 0 .. block_size {
		pad[i] ^= 0x36
	}
	m.inner.write(pad) or { panic(err) }
	for i in 0 .. block_size {
		pad[i] ^= 0x36 ^ 0x5c
	}
	m.outer.write(pad) or { panic(err) }
	for i in 0 .. block_size {
		pad[i] = 0
	}
	m.work.copy_from(m.inner)
	return m
}

// reset discards the message written so far, for example after an error, and
// starts a new one. The key is kept. `sum_into` already does this, so it is
// not needed between messages.
pub fn (mut m Hmac[D]) reset() {
	m.work.copy_from(m.inner)
}

// write adds `data` to the message. It never returns an error.
pub fn (mut m Hmac[D]) write(data []u8) !int {
	return m.work.write(data)
}

// sum_into writes the HMAC of the message written since the last `sum_into` or
// `reset` into the first `size()` bytes of `out`, without allocating, and starts
// a new message. It panics if `out` is shorter than `size()`.
pub fn (mut m Hmac[D]) sum_into(mut out []u8) {
	n := m.work.size()
	if out.len < n {
		panic('hmac: sum_into: `out` must be at least ${n} bytes long')
	}
	// HMAC = H((key ^ opad) || H((key ^ ipad) || message))
	m.work.checksum_into(mut out)
	m.work.copy_from(m.outer)
	m.work.write(out[..n]) or { panic(err) }
	m.work.checksum_into(mut out)
	// start the next message
	m.work.copy_from(m.inner)
}

// size returns the size of the HMAC in bytes, which is the size of the digest.
pub fn (m &Hmac[D]) size() int {
	return m.work.size()
}
