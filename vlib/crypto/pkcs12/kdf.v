module pkcs12

import crypto.sha1
import crypto.sha256

// pkcs12_kdf implements RFC 7292 Appendix B.2 password-based key derivation.
// `hash_name` is 'sha1' or 'sha256'. `id` is 1 (key), 2 (IV) or 3 (MAC).
// Every slice access is bounds-checked via helpers; no panics on bad input.
fn pkcs12_kdf(password []u8, salt []u8, iterations int, id u8, n int, hash_name string) ![]u8 {
	if iterations <= 0 {
		return error('pkcs12: bad iteration count')
	}
	if n <= 0 || n > 1024 {
		return error('pkcs12: bad derived length')
	}
	if id != 1 && id != 2 && id != 3 {
		return error('pkcs12: bad KDF id')
	}
	u := if hash_name == 'sha256' { 32 } else { 20 }
	v := 64
	if hash_name != 'sha1' && hash_name != 'sha256' {
		return error('pkcs12: unsupported KDF hash ${hash_name}')
	}
	// Diversifier: id repeated v times.
	mut d := []u8{len: v, init: id}
	// S: salt repeated to a multiple of v (empty stays empty).
	mut s := []u8{}
	if salt.len > 0 {
		sblocks := (salt.len + v - 1) / v
		s = []u8{cap: sblocks * v}
		for s.len < sblocks * v {
			need := sblocks * v - s.len
			if need >= salt.len {
				s << salt
			} else {
				s << salt[..need]
			}
		}
	}
	// P: password repeated to a multiple of v (empty stays empty).
	mut p := []u8{}
	if password.len > 0 {
		pblocks := (password.len + v - 1) / v
		p = []u8{cap: pblocks * v}
		for p.len < pblocks * v {
			need := pblocks * v - p.len
			if need >= password.len {
				p << password
			} else {
				p << password[..need]
			}
		}
	}
	mut i_block := []u8{}
	i_block << s
	i_block << p
	// Number of hash blocks to generate.
	c := (n + u - 1) / u
	mut out := []u8{cap: c * u}
	mut a := []u8{}
	mut b := []u8{len: v}
	for _ in 0 .. c {
		// A = Hash^iterations(D || I).
		mut msg := []u8{cap: d.len + i_block.len}
		msg << d
		msg << i_block
		a = if hash_name == 'sha256' { sha256.sum256(msg) } else { sha1.sum(msg) }
		for _ in 1 .. iterations {
			a = if hash_name == 'sha256' { sha256.sum256(a) } else { sha1.sum(a) }
		}
		// B = A repeated to fill v bytes.
		for k in 0 .. v {
			if a.len == 0 {
				return error('pkcs12: empty hash output')
			}
			b[k] = a[k % a.len]
		}
		// I = (I + B + 1) block-wise, big-endian with carry.
		if i_block.len > 0 {
			if i_block.len % v != 0 {
				return error('pkcs12: bad I length')
			}
			for j := 0; j < i_block.len; j += v {
				if j < 0 || j + v > i_block.len {
					return error('pkcs12: bad I block')
				}
				mut carry := u16(1)
				mut k := j + v - 1
				for {
					sum := u16(i_block[k]) + u16(b[k - j]) + carry
					i_block[k] = u8(sum & 0xff)
					carry = sum >> 8
					if k == j {
						break
					}
					k--
				}
			}
		}
		out << a
	}
	if out.len < n {
		return error('pkcs12: KDF short output')
	}
	return out[..n].clone()
}

// pkcs12_derive_key_iv derives the key and IV for legacy PKCS#12 PBE
// (always SHA-1 based) with length guards.
fn pkcs12_derive_key_iv(password []u8, salt []u8, iterations int, key_len int, iv_len int) !([]u8, []u8) {
	if key_len <= 0 || key_len > 64 || iv_len < 0 || iv_len > 64 {
		return error('pkcs12: bad key/iv length')
	}
	key := pkcs12_kdf(password, salt, iterations, 1, key_len, 'sha1')!
	iv := if iv_len > 0 {
		pkcs12_kdf(password, salt, iterations, 2, iv_len, 'sha1')!
	} else {
		[]u8{}
	}
	return key, iv
}

// pkcs12_derive_mac_key derives the MAC key with the MAC hash.
fn pkcs12_derive_mac_key(password []u8, salt []u8, iterations int, hash_name string) ![]u8 {
	u := if hash_name == 'sha256' {
		32
	} else if hash_name == 'sha1' {
		20
	} else {
		return error('pkcs12: unsupported MAC hash ${hash_name}')
	}
	return pkcs12_kdf(password, salt, iterations, 3, u, hash_name)
}
