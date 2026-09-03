// Algorithm-aware signing and verification helpers. These are the only
// places in the module that touch `crypto.ecdsa` and `crypto.ed25519`,
// so the signature/verification paths for new algorithms can be added
// here without rippling through the message types.
module cose

import crypto.ecdsa
import crypto.ed25519

// EcParams bundles the parameters that change between ES256/384/512:
// expected curve, OpenSSL NID, and coordinate byte width.
struct EcParams {
	curve      Curve
	nid        ecdsa.Nid
	coord_size int
}

// ec_params_for returns the curve / NID / coordinate size for an ECDSA
// COSE algorithm. Errors out for non-ECDSA algorithms.
fn ec_params_for(alg Algorithm) !EcParams {
	return match alg {
		.es256 {
			EcParams{
				curve:      .p_256
				nid:        .prime256v1
				coord_size: 32
			}
		}
		.es384 {
			EcParams{
				curve:      .p_384
				nid:        .secp384r1
				coord_size: 48
			}
		}
		.es512 {
			EcParams{
				curve:      .p_521
				nid:        .secp521r1
				coord_size: 66 // P-521 → ⌈521/8⌉ = 66
			}
		}
		else {
			error('cose: not an ECDSA algorithm: ${alg.name()}')
		}
	}
}

// check_ec_key validates that `key` is a usable EC2 key for `alg` and
// returns the matching EcParams. Used by both sign_with_key and
// verify_with_key to share the upfront validation.
fn check_ec_key(alg Algorithm, key Key) !EcParams {
	if key.kty != .ec2 {
		return error('cose: ${alg.name()} requires kty=EC2, got ${key.kty}')
	}
	params := ec_params_for(alg)!
	crv := key.crv or { return error('cose: EC2 key missing crv') }
	if crv != params.curve {
		return error('cose: ${alg.name()} requires crv=${params.curve}, got ${crv}')
	}
	return params
}

// check_okp_key validates that `key` is a usable OKP/Ed25519 key.
fn check_okp_key(key Key) ! {
	if key.kty != .okp {
		return error('cose: EdDSA requires kty=OKP, got ${key.kty}')
	}
	crv := key.crv or { return error('cose: OKP key missing crv') }
	if crv != .ed25519 {
		return error('cose: EdDSA requires crv=Ed25519, got ${crv}')
	}
}

// sign_with_key signs `to_be_signed` with `key`, producing a COSE-format
// signature (`R || S` for ECDSA, raw 64 bytes for Ed25519). The
// algorithm comes from `alg` rather than from the key so callers can
// reuse a key across multiple signing operations — but a key that
// declares its own `alg` MUST match: this catches accidental
// mismatches between key generation and signing intent.
fn sign_with_key(alg Algorithm, key Key, to_be_signed []u8) ![]u8 {
	if !alg.is_signature() {
		return UnsupportedAlgorithm{
			algorithm: alg
			context:   'signing'
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
	d := key.d or { return error('cose: signing requires a private key (missing d)') }

	match alg {
		.es256, .es384, .es512 {
			params := check_ec_key(alg, key)!
			priv := ecdsa.new_key_from_seed(d, ecdsa.CurveOptions{
				nid:        params.nid
				fixed_size: true
			})!
			defer {
				priv.free()
			}
			der := priv.sign(to_be_signed, ecdsa.SignerOpts{})!
			return der_to_raw(der, params.coord_size)!
		}
		.eddsa {
			check_okp_key(key)!
			x := key.x or { return error('cose: OKP key missing x (public key)') }
			if d.len != ed25519.seed_size {
				return error('cose: Ed25519 seed must be ${ed25519.seed_size} bytes, got ${d.len}')
			}
			if x.len != ed25519.public_key_size {
				return error('cose: Ed25519 public key must be ${ed25519.public_key_size} bytes, got ${x.len}')
			}
			// vlib/crypto/ed25519 expects the 64-byte (seed || public-key)
			// concatenation as PrivateKey.
			mut full := []u8{cap: ed25519.private_key_size}
			full << d
			full << x
			return ed25519.sign(full, to_be_signed)!
		}
		else {
			return UnsupportedAlgorithm{
				algorithm: alg
				context:   'signing'
			}
		}
	}
}

// verify_with_key checks that `signature` is a valid signature over
// `to_be_signed` under the COSE algorithm `alg` and the given key.
// Returns a `VerificationFailed` error if the check fails.
fn verify_with_key(alg Algorithm, key Key, to_be_signed []u8, signature []u8) ! {
	if !alg.is_signature() {
		return UnsupportedAlgorithm{
			algorithm: alg
			context:   'signature verification'
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
	match alg {
		.es256, .es384, .es512 {
			params := check_ec_key(alg, key)!
			x := key.x or { return error('cose: EC2 key missing x') }
			y := key.y or { return error('cose: EC2 key missing y') }
			if signature.len != 2 * params.coord_size {
				return VerificationFailed{
					algorithm: alg
				}
			}
			der_sig := raw_to_der(signature, params.coord_size)!
			spki := build_ec_spki(params.curve, x, y)!
			pubkey := ecdsa.pubkey_from_bytes(spki)!
			defer {
				pubkey.free()
			}
			ok := pubkey.verify(to_be_signed, der_sig, ecdsa.SignerOpts{}) or {
				return VerificationFailed{
					algorithm: alg
				}
			}
			if !ok {
				return VerificationFailed{
					algorithm: alg
				}
			}
		}
		.eddsa {
			check_okp_key(key)!
			x := key.x or { return error('cose: OKP key missing x') }
			if x.len != ed25519.public_key_size {
				return error('cose: Ed25519 public key must be ${ed25519.public_key_size} bytes')
			}
			ok := ed25519.verify(x, to_be_signed, signature) or {
				return VerificationFailed{
					algorithm: alg
				}
			}
			if !ok {
				return VerificationFailed{
					algorithm: alg
				}
			}
		}
		else {
			return UnsupportedAlgorithm{
				algorithm: alg
				context:   'signature verification'
			}
		}
	}
}

// build_ec_spki assembles a SubjectPublicKeyInfo DER blob (RFC 5480) for
// an uncompressed EC public point. This is the format consumed by
// `ecdsa.pubkey_from_bytes`. We do this in pure V to avoid having to
// add new C bindings just to load a public key from raw coordinates.
fn build_ec_spki(crv Curve, x []u8, y []u8) ![]u8 {
	// id-ecPublicKey OID = 1.2.840.10045.2.1
	id_ec_public_key := [u8(0x06), 0x07, 0x2A, 0x86, 0x48, 0xCE, 0x3D, 0x02, 0x01]
	// Per-curve OID:
	curve_oid := match crv {
		.p_256 { [u8(0x06), 0x08, 0x2A, 0x86, 0x48, 0xCE, 0x3D, 0x03, 0x01, 0x07] }
		.p_384 { [u8(0x06), 0x05, 0x2B, 0x81, 0x04, 0x00, 0x22] }
		.p_521 { [u8(0x06), 0x05, 0x2B, 0x81, 0x04, 0x00, 0x23] }
		else { return error('cose: not an EC2 curve for SPKI: ${crv}') }
	}

	coord_size := match crv {
		.p_256 { 32 }
		.p_384 { 48 }
		.p_521 { 66 }
		else { return error('cose: not an EC2 curve for SPKI: ${crv}') }
	}

	if x.len > coord_size || y.len > coord_size {
		return MalformedMessage{
			reason: 'EC2 coordinates exceed curve size'
		}
	}

	// Uncompressed point: 0x04 || X (left-padded) || Y (left-padded).
	mut point := []u8{len: 1 + 2 * coord_size}
	point[0] = 0x04
	x_off := 1 + (coord_size - x.len)
	for i in 0 .. x.len {
		point[x_off + i] = x[i]
	}
	y_off := 1 + coord_size + (coord_size - y.len)
	for i in 0 .. y.len {
		point[y_off + i] = y[i]
	}

	// AlgorithmIdentifier ::= SEQUENCE { id_ec_public_key, curve_oid }
	mut alg_id_body := []u8{cap: id_ec_public_key.len + curve_oid.len}
	alg_id_body << id_ec_public_key
	alg_id_body << curve_oid
	mut alg_id := []u8{cap: 2 + alg_id_body.len}
	alg_id << 0x30
	alg_id << encode_der_length(alg_id_body.len)
	alg_id << alg_id_body

	// BIT STRING: 0x03 LEN UNUSED_BITS || POINT
	mut bit_string := []u8{cap: 3 + point.len}
	bit_string << 0x03
	bit_string << encode_der_length(point.len + 1)
	bit_string << 0x00 // 0 unused bits in the trailing octet
	bit_string << point

	// SubjectPublicKeyInfo ::= SEQUENCE { alg_id, bit_string }
	mut spki_body := []u8{cap: alg_id.len + bit_string.len}
	spki_body << alg_id
	spki_body << bit_string
	mut spki := []u8{cap: 2 + spki_body.len}
	spki << 0x30
	spki << encode_der_length(spki_body.len)
	spki << spki_body
	return spki
}
