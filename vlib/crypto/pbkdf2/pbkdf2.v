// Based off: https://golang.org/x/crypto/pbkdf2
module pbkdf2

import crypto.hmac
import crypto.sha1
import crypto.sha256
import crypto.sha512
import hash

// key derives a key from the password, salt and iteration count
// `h` selects the hash used by HMAC: a digest from `crypto.sha1`, `crypto.sha256`
// (SHA-224, SHA-256) or `crypto.sha512` (SHA-384, SHA-512, SHA-512/224, SHA-512/256).
// example pbkdf2.key('test'.bytes(), '123456'.bytes(), 1000, 64, sha512.new())
pub fn key(password []u8, salt []u8, count int, key_length int, h hash.Hash) ![]u8 {
	match h {
		sha1.Digest {
			mut mac := hmac.new_hmac(sha1.new, password)
			return derive(mut mac, salt, count, key_length)
		}
		sha256.Digest {
			new_digest := if h.size() == sha256.size224 { sha256.new224 } else { sha256.new }
			mut mac := hmac.new_hmac(new_digest, password)
			return derive(mut mac, salt, count, key_length)
		}
		sha512.Digest {
			new_digest := match h.size() {
				sha512.size384 { sha512.new384 }
				sha512.size256 { sha512.new512_256 }
				sha512.size224 { sha512.new512_224 }
				else { sha512.new }
			}
			mut mac := hmac.new_hmac(new_digest, password)
			return derive(mut mac, salt, count, key_length)
		}
		else {
			return error('Unsupported hash')
		}
	}
}

// derive implements PBKDF2 with `mac`, which is keyed with the password, as the
// pseudorandom function. `hmac.Hmac` processes the key once, so the iteration
// loop does not allocate.
@[direct_array_access]
fn derive[D](mut mac hmac.Hmac[D], salt []u8, count int, key_length int) []u8 {
	hash_length := mac.size()
	block_count := (key_length + hash_length - 1) / hash_length
	mut output := []u8{cap: block_count * hash_length}
	mut u := []u8{len: hash_length}
	mut xorsum := []u8{len: hash_length}
	mut buf := []u8{len: 4}
	for i := 1; i <= block_count; i++ {
		buf[0] = u8(i >> 24)
		buf[1] = u8(i >> 16)
		buf[2] = u8(i >> 8)
		buf[3] = u8(i)
		// U_1 = HMAC(password, salt || INT(i))
		mac.write(salt) or { panic(err) }
		mac.write(buf) or { panic(err) }
		mac.sum_into(mut u)
		copy(mut xorsum, u)
		for j := 1; j < count; j++ {
			// U_j = HMAC(password, U_{j-1})
			mac.write(u) or { panic(err) }
			mac.sum_into(mut u)
			for k in 0 .. hash_length {
				xorsum[k] ^= u[k]
			}
		}
		output << xorsum
	}
	return output[..key_length]
}
