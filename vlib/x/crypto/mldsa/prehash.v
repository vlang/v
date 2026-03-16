// FIPS 204 s. 5.4: Pre-Hash ML-DSA
// signs PH(M) instead of M directly, prefer using pure ml-dsa when possible

module mldsa

import crypto.sha256
import crypto.sha512
import crypto.sha3

// algo. 4/5: DER-encoded OID for each pre-hash function
// all under arc 2.16.840.1.101.3.4.2
// joint-iso-itu-t(2) country(16) us(840) organization(1) gov(101) csor(3) nistAlgorithm(4) hashAlgs(2)
fn prehash_oid(ph PreHash) []u8 {
	suffix := match ph {
		.sha2_256 { u8(0x01) }
		.sha2_384 { u8(0x02) }
		.sha2_512 { u8(0x03) }
		.sha2_224 { u8(0x04) }
		.sha2_512_224 { u8(0x05) }
		.sha2_512_256 { u8(0x06) }
		.sha3_224 { u8(0x07) }
		.sha3_256 { u8(0x08) }
		.sha3_384 { u8(0x09) }
		.sha3_512 { u8(0x0a) }
		.shake_128 { u8(0x0b) }
		.shake_256 { u8(0x0c) }
		// unreachable, called from mu_prehash, which is called from sign when ph != .none
		.none { panic('mldsa: prehash_oid called with .none') }
	}
	return [u8(0x06), 0x09, 0x60, 0x86, 0x48, 0x01, 0x65, 0x03, 0x04, 0x02, suffix]
}

// algo. 4, lines 10-22: PH(M) for the given hash function
fn prehash_message(msg []u8, ph PreHash) []u8 {
	return match ph {
		.sha2_224 { sha256.sum224(msg) }
		.sha2_256 { sha256.sum256(msg) }
		.sha2_384 { sha512.sum384(msg) }
		.sha2_512 { sha512.sum512(msg) }
		.sha2_512_224 { sha512.sum512_224(msg) }
		.sha2_512_256 { sha512.sum512_256(msg) }
		.sha3_224 { sha3.sum224(msg) }
		.sha3_256 { sha3.sum256(msg) }
		.sha3_384 { sha3.sum384(msg) }
		.sha3_512 { sha3.sum512(msg) }
		.shake_128 { sha3.shake128(msg, 32) }
		.shake_256 { sha3.shake256(msg, 64) }
		// unreachable, called from mu_prehash, which is called from sign when ph != .none
		.none { panic('mldsa: prehash_message called with .none') }
	}
}

// algo. 4, line 23: M' = 0x01 || |ctx| || ctx || OID || PH(M)
// algo. 7, line 6: mu = H(tr || M')
// compute_mu_prehash computes mu for prehash mode: H(tr || 0x01 || |ctx| || ctx || OID || PH(msg), 64).
pub fn compute_mu_prehash(tr []u8, msg []u8, context string, ph PreHash) [64]u8 {
	mut h := sha3.new_shake256()
	h.write(tr)
	h.write([u8(0x01)]) // domain sep
	h.write([u8(context.len)])
	h.write(context.bytes())
	h.write(prehash_oid(ph))
	h.write(prehash_message(msg, ph))
	return slice_to_64(h.read(64))
}
