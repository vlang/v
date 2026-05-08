// Algorithm-aware MAC computation. Sibling of `signing.v`. Only this
// file imports `crypto.hmac` and the SHA-2 modules; the rest of the
// MAC handling code goes through `compute_mac` / `verify_mac`.
module cose

import crypto.hmac
import crypto.sha256
import crypto.sha512

// compute_mac returns the MAC tag (already truncated for HMAC 256/64)
// for `data` under `alg` using the symmetric `key`.
fn compute_mac(alg Algorithm, key Key, data []u8) ![]u8 {
	if !alg.is_mac() {
		return UnsupportedAlgorithm{
			algorithm: alg
			context:   'MAC'
		}
	}
	if key_alg := key.alg {
		if key_alg != alg {
			return AlgorithmMismatch{
				expected: key_alg
				got:      alg
			}
		}
	}
	if key.kty != .symmetric {
		return error('cose: ${alg.name()} requires kty=Symmetric, got ${key.kty}')
	}
	k := key.k or { return error('cose: symmetric key missing k') }

	tag := match alg {
		.hmac_256_64, .hmac_256_256 {
			hmac.new(k, data, sha256.sum, sha256.block_size)
		}
		.hmac_384_384 {
			hmac.new(k, data, sha512.sum384, sha512.block_size)
		}
		.hmac_512_512 {
			hmac.new(k, data, sha512.sum512, sha512.block_size)
		}
		else {
			// Unreachable: `alg.is_mac()` above ruled out non-MAC values
			// and the four MAC variants are all enumerated. Kept so the
			// match stays exhaustive over the Algorithm enum.
			return UnsupportedAlgorithm{
				algorithm: alg
				context:   'MAC'
			}
		}
	}

	// HMAC 256/64 (RFC 9053 §3.1) truncates to the leftmost 8 bytes.
	if alg == .hmac_256_64 {
		return tag[..8].clone()
	}
	return tag
}

// mac_verify checks that `tag` matches the MAC of `data` under `alg`
// and `key`, using a constant-time comparison.
fn mac_verify(alg Algorithm, key Key, data []u8, tag []u8) ! {
	expected := compute_mac(alg, key, data)!
	if !hmac.equal(expected, tag) {
		return VerificationFailed{
			algorithm: alg
		}
	}
}
