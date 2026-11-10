module rsa

import crypto
import crypto.rand
import crypto.sha256
import crypto.sha512
import crypto.subtle

// The DER encodings of the DigestInfo prefix of RFC 8017 §9.2 for each hash:
// the AlgorithmIdentifier of the hash, followed by the header of the OCTET
// STRING that holds the digest.
const digest_info_sha256 = [u8(0x30), 0x31, 0x30, 0x0d, 0x06, 0x09, 0x60, 0x86, 0x48, 0x01, 0x65,
	0x03, 0x04, 0x02, 0x01, 0x05, 0x00, 0x04, 0x20]!
const digest_info_sha384 = [u8(0x30), 0x41, 0x30, 0x0d, 0x06, 0x09, 0x60, 0x86, 0x48, 0x01, 0x65,
	0x03, 0x04, 0x02, 0x02, 0x05, 0x00, 0x04, 0x30]!
const digest_info_sha512 = [u8(0x30), 0x51, 0x30, 0x0d, 0x06, 0x09, 0x60, 0x86, 0x48, 0x01, 0x65,
	0x03, 0x04, 0x02, 0x03, 0x05, 0x00, 0x04, 0x40]!

// hash_size returns the digest length of a hash supported by this module.
fn hash_size(h crypto.Hash) !int {
	return match h {
		.sha256 { sha256.size }
		.sha384 { sha512.size384 }
		.sha512 { sha512.size }
		else { error('crypto.rsa: unsupported hash ${h}, expected sha256, sha384 or sha512') }
	}
}

// hash_sum returns the digest of `data` with a hash supported by this module.
fn hash_sum(h crypto.Hash, data []u8) ![]u8 {
	return match h {
		.sha256 { sha256.sum256(data) }
		.sha384 { sha512.sum384(data) }
		.sha512 { sha512.sum512(data) }
		else { error('crypto.rsa: unsupported hash ${h}, expected sha256, sha384 or sha512') }
	}
}

// check_digest validates the length of `digest` for `h`.
fn check_digest(h crypto.Hash, digest []u8) ! {
	size := hash_size(h)!
	if digest.len != size {
		return error('crypto.rsa: digest is ${digest.len} bytes, ${h} digests are ${size} bytes')
	}
}

// emsa_pkcs1v15_encode returns the EMSA-PKCS1-v1_5 encoding of `digest` in
// `em_len` bytes (RFC 8017 §9.2): 0x00 0x01, padding bytes 0xff, 0x00, then
// the DigestInfo.
fn emsa_pkcs1v15_encode(h crypto.Hash, digest []u8, em_len int) ![]u8 {
	check_digest(h, digest)!
	prefix := match h {
		.sha256 { digest_info_sha256[..] }
		.sha384 { digest_info_sha384[..] }
		else { digest_info_sha512[..] }
	}
	t_len := prefix.len + digest.len
	if em_len < t_len + 11 {
		return error('crypto.rsa: key too short for a ${h} PKCS #1 v1.5 signature')
	}
	mut em := []u8{len: em_len, init: 0xff}
	em[0] = 0x00
	em[1] = 0x01
	em[em_len - t_len - 1] = 0x00
	copy(mut em[em_len - t_len..], prefix)
	copy(mut em[em_len - digest.len..], digest)
	return em
}

// mgf1_xor xors `out` with the MGF1 mask generated from `seed` (RFC 8017 §B.2.1).
fn mgf1_xor(mut out []u8, h crypto.Hash, seed []u8) ! {
	mut counter := u32(0)
	mut done := 0
	for done < out.len {
		mut input := seed.clone()
		input << [u8(counter >> 24), u8(counter >> 16), u8(counter >> 8), u8(counter)]
		block := hash_sum(h, input)!
		for i := 0; i < block.len && done < out.len; i++ {
			out[done] ^= block[i]
			done++
		}
		counter++
	}
}

// emsa_pss_encode returns the EMSA-PSS encoding of `digest` for a modulus of
// `mod_bits` bits (RFC 8017 §9.1.1), with a random salt of `salt_len` bytes
// and MGF1 over the same hash.
fn emsa_pss_encode(h crypto.Hash, digest []u8, mod_bits int, salt_len int) ![]u8 {
	check_digest(h, digest)!
	h_len := digest.len
	em_bits := mod_bits - 1
	em_len := (em_bits + 7) / 8
	if salt_len < 0 || salt_len > em_len - h_len - 2 {
		return error('crypto.rsa: key too short for a ${h} PSS signature with a ${salt_len}-byte salt')
	}
	salt := rand.bytes(salt_len)!
	mut m_prime := []u8{len: 8}
	m_prime << digest
	m_prime << salt
	hh := hash_sum(h, m_prime)!
	// DB = PS || 0x01 || salt, masked with MGF1(H).
	db_len := em_len - h_len - 1
	mut db := []u8{len: db_len}
	db[db_len - salt_len - 1] = 0x01
	copy(mut db[db_len - salt_len..], salt)
	mgf1_xor(mut db, h, hh)!
	db[0] &= u8(0xff >> u32(8 * em_len - em_bits))
	mut em := db.clone()
	em << hh
	em << u8(0xbc)
	return em
}

// emsa_pss_verify checks that `em` is the EMSA-PSS encoding of `digest` for a
// modulus of `mod_bits` bits (RFC 8017 §9.1.2). A `salt_len` of
// `pss_salt_length_auto` accepts any salt length.
fn emsa_pss_verify(h crypto.Hash, digest []u8, em []u8, mod_bits int, salt_len int) !bool {
	check_digest(h, digest)!
	h_len := digest.len
	em_bits := mod_bits - 1
	em_len := (em_bits + 7) / 8
	if em.len != em_len || em_len < h_len + 2 || (salt_len >= 0 && salt_len > em_len - h_len - 2) {
		return false
	}
	if em[em_len - 1] != 0xbc {
		return false
	}
	db_len := em_len - h_len - 1
	mut db := em[..db_len].clone()
	hh := em[db_len..em_len - 1]
	top_mask := u8(0xff >> u32(8 * em_len - em_bits))
	if db[0] & ~top_mask != 0 {
		return false
	}
	mgf1_xor(mut db, h, hh)!
	db[0] &= top_mask
	// DB must be zero bytes, then 0x01, then the salt.
	mut one := 0
	for one < db_len && db[one] == 0 {
		one++
	}
	if one == db_len || db[one] != 0x01 {
		return false
	}
	actual_salt_len := db_len - one - 1
	if salt_len >= 0 && actual_salt_len != salt_len {
		return false
	}
	mut m_prime := []u8{len: 8}
	m_prime << digest
	m_prime << db[one + 1..]
	return subtle.constant_time_compare(hash_sum(h, m_prime)!, hh) == 1
}
