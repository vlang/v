// Key material used by `sign` and `verify`. The struct is intentionally
// algorithm-tagged - one Key value carries both its algorithm and the
// raw octets, so the high-level helpers can pick the right primitive
// without the caller having to repeat themselves.
//
// Use the typed constructors (Key.hmac_sha256, Key.ed25519_private,
// Key.ecdsa_p256_public, etc.) - never build a Key by struct literal.
module signature

import crypto.ecdsa
import crypto.pem

// Key is a tagged blob of key material. Public/private distinction is
// in `is_private`; the on-the-wire layout of `bytes` depends on
// `algorithm` and is documented per constructor below.
pub struct Key {
pub:
	algorithm  Algorithm
	is_private bool
	// bytes layout per algorithm:
	//   .hmac_sha256       → the symmetric secret
	//   .ed25519, private  → 32-byte seed (RFC 8032 §5.1.5)
	//   .ed25519, public   → 32-byte x coordinate
	//   .ecdsa_*, private  → x || y || d (each at curve byte size)
	//   .ecdsa_*, public   → x || y       (each at curve byte size)
	bytes []u8
	// keyid, if set, is what the high-level helpers will emit as the
	// `keyid` signature parameter when no explicit keyid is passed.
	// Not part of the cryptographic identity - just routing metadata.
	keyid ?string
}

// Key.hmac_sha256 builds a symmetric key for the hmac-sha256 algorithm.
// `secret` must not be empty. RFC 9421 §3.3.3 recommends at least
// 256 bits of entropy.
pub fn Key.hmac_sha256(secret []u8) Key {
	return Key{
		algorithm: .hmac_sha256
		bytes:     secret
	}
}

// Key.ed25519_private wraps a 32-byte Ed25519 seed (RFC 8032 §5.1.5).
// The corresponding public key can be derived on demand by the signer.
pub fn Key.ed25519_private(seed []u8) Key {
	return Key{
		algorithm:  .ed25519
		is_private: true
		bytes:      seed
	}
}

// Key.ed25519_public wraps the 32-byte Ed25519 public x-coordinate.
pub fn Key.ed25519_public(x []u8) Key {
	return Key{
		algorithm: .ed25519
		bytes:     x
	}
}

// Key.ecdsa_p256_private wraps an ECDSA P-256 private key as raw
// (x, y, d) coordinates. Each coordinate is zero-padded to 32 bytes
// (the curve byte size).
pub fn Key.ecdsa_p256_private(x []u8, y []u8, d []u8) Key {
	return Key{
		algorithm:  .ecdsa_p256_sha256
		is_private: true
		bytes:      concat3(x, y, d)
	}
}

// Key.ecdsa_p256_public wraps an ECDSA P-256 public key as raw (x, y).
pub fn Key.ecdsa_p256_public(x []u8, y []u8) Key {
	return Key{
		algorithm: .ecdsa_p256_sha256
		bytes:     concat3(x, y, []u8{})
	}
}

// Key.ecdsa_p384_private wraps an ECDSA P-384 private key as raw
// (x, y, d) coordinates. Each coordinate is zero-padded to 48 bytes.
pub fn Key.ecdsa_p384_private(x []u8, y []u8, d []u8) Key {
	return Key{
		algorithm:  .ecdsa_p384_sha384
		is_private: true
		bytes:      concat3(x, y, d)
	}
}

// Key.ecdsa_p384_public wraps an ECDSA P-384 public key as raw (x, y).
pub fn Key.ecdsa_p384_public(x []u8, y []u8) Key {
	return Key{
		algorithm: .ecdsa_p384_sha384
		bytes:     concat3(x, y, []u8{})
	}
}

// with_keyid returns a copy of the Key with `keyid` set. Convenience
// for fluent construction at call sites.
pub fn (k Key) with_keyid(keyid string) Key {
	return Key{
		algorithm:  k.algorithm
		is_private: k.is_private
		bytes:      k.bytes
		keyid:      keyid
	}
}

fn concat3(a []u8, b []u8, c []u8) []u8 {
	mut out := []u8{cap: a.len + b.len + c.len}
	out << a
	out << b
	out << c
	return out
}

// Key.from_pem decodes a PEM-encoded key and returns a Key tagged with
// the algorithm inferred from the embedded OID. Supports the four
// PEM shapes commonly emitted by `openssl genpkey` / `openssl ec`:
//
//   * `-----BEGIN PRIVATE KEY-----`     (PKCS#8 — Ed25519 or ECDSA)
//   * `-----BEGIN EC PRIVATE KEY-----`  (SEC1 — ECDSA)
//   * `-----BEGIN PUBLIC KEY-----`      (SPKI — Ed25519 or ECDSA)
//
// HMAC keys never come as PEM (they're raw shared secrets) — call
// `Key.hmac_sha256` directly with the bytes for those.
pub fn Key.from_pem(pem_text string) !Key {
	block := pem.decode_only(pem_text) or {
		return MalformedMessage{
			reason: 'PEM decode failed (missing or malformed BEGIN/END markers)'
		}
	}
	der := block.data
	if contains_oid_ed25519(der) {
		return match block.block_type {
			'PRIVATE KEY' {
				parse_ed25519_pkcs8(der)!
			}
			'PUBLIC KEY' {
				parse_ed25519_spki(der)!
			}
			else {
				return MalformedMessage{
					reason: 'unexpected PEM block "${block.block_type}" for an Ed25519 key'
				}
			}
		}
	}
	// ECDSA path - delegate parsing to vlib/crypto/ecdsa, then extract
	// raw coordinates so the Key struct stays algorithm-tagged.
	return match block.block_type {
		'PUBLIC KEY' {
			ecdsa_key_from_pem_public(pem_text)!
		}
		'EC PRIVATE KEY', 'PRIVATE KEY' {
			ecdsa_key_from_pem_private(pem_text)!
		}
		else {
			return MalformedMessage{
				reason: 'unsupported PEM block "${block.block_type}" - expected PRIVATE KEY, EC PRIVATE KEY, or PUBLIC KEY'
			}
		}
	}
}

// OID 1.3.101.112 (id-Ed25519, RFC 8410 §3) - the byte sequence
// `06 03 2B 65 70` is unambiguous in any PKCS#8 / SPKI Ed25519 blob.
fn contains_oid_ed25519(b []u8) bool {
	needle := [u8(0x06), 0x03, 0x2B, 0x65, 0x70]
	if b.len < needle.len {
		return false
	}
	for i in 0 .. b.len - needle.len + 1 {
		mut ok := true
		for j in 0 .. needle.len {
			if b[i + j] != needle[j] {
				ok = false
				break
			}
		}
		if ok {
			return true
		}
	}
	return false
}

// parse_ed25519_pkcs8 extracts the 32-byte seed from the canonical
// RFC 8410 §7 PrivateKeyInfo encoding. The DER is fully constrained
// for Ed25519 keys (no optional fields), so we recognise the byte
// pattern rather than running a general ASN.1 parser.
fn parse_ed25519_pkcs8(der []u8) !Key {
	prefix := [u8(0x30), 0x2E, 0x02, 0x01, 0x00, 0x30, 0x05, 0x06, 0x03, 0x2B, 0x65, 0x70, 0x04,
		0x22, 0x04, 0x20]
	if der.len != 48 || !has_prefix(der, prefix) {
		return MalformedMessage{
			reason: 'Ed25519 PKCS#8 private key must follow the canonical RFC 8410 §7 encoding'
		}
	}
	return Key.ed25519_private(der[16..48].clone())
}

// parse_ed25519_spki extracts the 32-byte x coordinate from the
// canonical SubjectPublicKeyInfo encoding (RFC 8410 §4).
fn parse_ed25519_spki(der []u8) !Key {
	prefix := [u8(0x30), 0x2A, 0x30, 0x05, 0x06, 0x03, 0x2B, 0x65, 0x70, 0x03, 0x21, 0x00]
	if der.len != 44 || !has_prefix(der, prefix) {
		return MalformedMessage{
			reason: 'Ed25519 SPKI public key must follow the canonical RFC 8410 §4 encoding'
		}
	}
	return Key.ed25519_public(der[12..44].clone())
}

fn has_prefix(b []u8, prefix []u8) bool {
	if b.len < prefix.len {
		return false
	}
	for i in 0 .. prefix.len {
		if b[i] != prefix[i] {
			return false
		}
	}
	return true
}

fn ecdsa_key_from_pem_private(pem_text string) !Key {
	priv_obj := ecdsa.privkey_from_string(pem_text) or {
		return MalformedMessage{
			reason: 'ECDSA PEM parse failed: ${err.msg()}'
		}
	}
	d := priv_obj.bytes()!
	pub_obj := priv_obj.public_key()!
	priv_obj.free()
	xy := pub_obj.bytes()!
	pub_obj.free()
	return ecdsa_key_from_xy_d(xy, d, true)!
}

fn ecdsa_key_from_pem_public(pem_text string) !Key {
	pub_obj := ecdsa.pubkey_from_string(pem_text) or {
		return MalformedMessage{
			reason: 'ECDSA PEM parse failed: ${err.msg()}'
		}
	}
	xy := pub_obj.bytes()!
	pub_obj.free()
	return ecdsa_key_from_xy_d(xy, []u8{}, false)!
}

// ecdsa_key_from_xy_d takes the SEC1 uncompressed point (`xy` =
// `0x04 || x || y`) and an optional `d` and selects the matching
// `Key.ecdsa_p256_*` / `Key.ecdsa_p384_*` constructor by point size.
fn ecdsa_key_from_xy_d(xy []u8, d []u8, is_priv bool) !Key {
	if xy.len < 1 || xy[0] != 0x04 {
		return MalformedMessage{
			reason: 'ECDSA public key must be SEC1 uncompressed (0x04 || x || y)'
		}
	}
	coord := (xy.len - 1) / 2
	x := xy[1..1 + coord]
	y := xy[1 + coord..1 + coord * 2]
	return match coord {
		32 {
			if is_priv {
				Key.ecdsa_p256_private(x, y, d)
			} else {
				Key.ecdsa_p256_public(x, y)
			}
		}
		48 {
			if is_priv {
				Key.ecdsa_p384_private(x, y, d)
			} else {
				Key.ecdsa_p384_public(x, y)
			}
		}
		else {
			return UnsupportedAlgorithm{
				name: 'ECDSA curve with ${coord * 8}-bit coordinates'
			}
		}
	}
}
