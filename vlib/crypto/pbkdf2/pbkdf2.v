// Based off: https://golang.org/x/crypto/pbkdf2
module pbkdf2

import crypto.hmac
import crypto.sha256
import crypto.sha512
import hash

// key derives a key from the password, salt and iteration count
// example pbkdf2.key('test'.bytes(), '123456'.bytes(), 1000, 64, sha512.new())
pub fn key(password []u8, salt []u8, count int, key_length int, h hash.Hash) ![]u8 {
	mut fun := fn (b []u8) []u8 {
		return []u8{}
	}
	mut block_size := 0
	mut size := 0
	match h {
		sha256.Digest {
			block_size = h.block_size()
			size = h.size()
			if size == sha256.size224 {
				fun = sha256.sum224
			} else {
				fun = sha256.sum256
			}
		}
		sha512.Digest {
			block_size = h.block_size()
			size = h.size()
			if size == sha512.size384 {
				fun = sha512.sum384
			} else {
				fun = sha512.sum512
			}
		}
		else {
			panic('Unsupported hash')
		}
	}

	hash_length := size
	block_count := (key_length + hash_length - 1) / hash_length
	mut output := []u8{}
	mut last := []u8{}
	mut buf := []u8{len: 4}
	for i := 1; i <= block_count; i++ {
		last.clear()
		last << salt

		buf[0] = u8(i >> 24)
		buf[1] = u8(i >> 16)
		buf[2] = u8(i >> 8)
		buf[3] = u8(i)

		last << buf
		mut xorsum := hmac.new(password, last, fun, block_size)
		mut last_hash := xorsum.clone()
		for j := 1; j < count; j++ {
			last_hash = hmac.new(password, last_hash, fun, block_size)
			for k in 0 .. xorsum.len {
				xorsum[k] ^= last_hash[k]
			}
		}
		output << xorsum
	}
	return output[..key_length]
}
