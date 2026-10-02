// Based off: https://golang.org/x/crypto/pbkdf2
module pbkdf2

import crypto.sha256
import crypto.sha512
import hash

// key derives a key from the password, salt and iteration count
// example pbkdf2.key('test'.bytes(), '123456'.bytes(), 1000, 64, sha512.new())
pub fn key(password []u8, salt []u8, count int, key_length int, h hash.Hash) ![]u8 {
	match h {
		sha256.Digest {
			new_digest := if h.size() == sha256.size224 { sha256.new224 } else { sha256.new }
			mut inner := new_digest()
			mut outer := new_digest()
			mut work := new_digest()
			return derive(mut inner, mut outer, mut work, password, salt, count, key_length)
		}
		sha512.Digest {
			new_digest := match h.size() {
				sha512.size384 { sha512.new384 }
				sha512.size256 { sha512.new512_256 }
				sha512.size224 { sha512.new512_224 }
				else { sha512.new }
			}
			mut inner := new_digest()
			mut outer := new_digest()
			mut work := new_digest()
			return derive(mut inner, mut outer, mut work, password, salt, count, key_length)
		}
		else {
			return error('Unsupported hash')
		}
	}
}

// derive implements PBKDF2 with HMAC-D as the pseudorandom function.
// `inner`, `outer` and `work` must be fresh digests of the same kind.
// The HMAC key is processed once: `inner` and `outer` keep the states after
// absorbing `K ^ ipad` and `K ^ opad`, and every HMAC computation restarts
// `work` from them, so the iteration loop does not allocate.
@[direct_array_access]
fn derive[D](mut inner D, mut outer D, mut work D, password []u8, salt []u8, count int, key_length int) []u8 {
	hash_length := work.size()
	block_size := work.block_size()
	// HMAC key, padded with zeros to the block size (RFC 2104). Keys longer
	// than the block size are hashed first.
	mut pad := []u8{len: block_size}
	if password.len > block_size {
		work.write(password) or { panic(err) }
		work.checksum_into(mut pad)
	} else {
		copy(mut pad, password)
	}
	for i in 0 .. block_size {
		pad[i] ^= 0x36
	}
	inner.write(pad) or { panic(err) }
	for i in 0 .. block_size {
		pad[i] ^= 0x36 ^ 0x5c
	}
	outer.write(pad) or { panic(err) }
	for i in 0 .. block_size {
		pad[i] = 0
	}

	block_count := (key_length + hash_length - 1) / hash_length
	mut output := []u8{cap: block_count * hash_length}
	mut u := []u8{len: hash_length}
	mut t := []u8{len: hash_length}
	mut xorsum := []u8{len: hash_length}
	mut buf := []u8{len: 4}
	for i := 1; i <= block_count; i++ {
		buf[0] = u8(i >> 24)
		buf[1] = u8(i >> 16)
		buf[2] = u8(i >> 8)
		buf[3] = u8(i)
		// U_1 = HMAC(password, salt || INT(i))
		work.copy_from(inner)
		work.write(salt) or { panic(err) }
		work.write(buf) or { panic(err) }
		finish_hmac(mut work, outer, mut t, mut u)
		copy(mut xorsum, u)
		for j := 1; j < count; j++ {
			// U_j = HMAC(password, U_{j-1})
			work.copy_from(inner)
			work.write(u) or { panic(err) }
			finish_hmac(mut work, outer, mut t, mut u)
			for k in 0 .. hash_length {
				xorsum[k] ^= u[k]
			}
		}
		output << xorsum
	}
	return output[..key_length]
}

// finish_hmac completes an HMAC whose message was written to `work`, which
// started from the inner state. It writes the inner hash to `t` and the HMAC
// to `out`.
fn finish_hmac[D](mut work D, outer D, mut t []u8, mut out []u8) {
	work.checksum_into(mut t)
	work.copy_from(outer)
	work.write(t) or { panic(err) }
	work.checksum_into(mut out)
}
