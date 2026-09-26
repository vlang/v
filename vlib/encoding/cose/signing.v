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

fn ed25519_key_pair_matches(x []u8, d []u8) bool {
	return ed25519.new_key_from_seed(d).public_key().equal(x)
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
	crv := key.crv or {
		if raw_curve := key.raw_curve {
			return error('cose: EC2 key uses unsupported curve ${raw_curve}')
		}
		if raw_curve_text := key.raw_curve_text {
			return error('cose: EC2 key uses unsupported curve "${raw_curve_text}"')
		}
		return error('cose: EC2 key missing crv')
	}

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
	crv := key.crv or {
		if raw_curve := key.raw_curve {
			return error('cose: OKP key uses unsupported curve ${raw_curve}')
		}
		if raw_curve_text := key.raw_curve_text {
			return error('cose: OKP key uses unsupported curve "${raw_curve_text}"')
		}
		return error('cose: OKP key missing crv')
	}

	if crv != .ed25519 {
		return error('cose: EdDSA requires crv=Ed25519, got ${crv}')
	}
}

fn check_ec_public_coordinates(priv ecdsa.PrivateKey, params EcParams, key Key) ! {
	x := key.x or { return error('cose: EC2 private key missing x') }
	y := key.y or { return error('cose: EC2 private key missing y') }
	advertised := ec_uncompressed_point(params.coord_size, x, y)!
	derived_public := priv.public_key()!
	defer {
		derived_public.free()
	}
	if derived_public.uncompressed_bytes()! != advertised {
		return error('cose: EC2 public coordinates x/y do not match private scalar d')
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
	key.check_algorithm(alg)!
	key.check_operation(.sign)!
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
			check_ec_public_coordinates(priv, params, key)!
			der := priv.sign(to_be_signed, ecdsa.SignerOpts{})!
			return der_to_raw(der, params.coord_size)!
		}
		.eddsa {
			check_okp_key(key)!
			if d.len != ed25519.seed_size {
				return error('cose: Ed25519 seed must be ${ed25519.seed_size} bytes, got ${d.len}')
			}
			full := ed25519.new_key_from_seed(d)
			if x := key.x {
				if x.len != ed25519.public_key_size {
					return error('cose: Ed25519 public key must be ${ed25519.public_key_size} bytes, got ${x.len}')
				}
				if !full.public_key().equal(x) {
					return error('cose: Ed25519 public key x does not match private seed d')
				}
			}
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
	key.check_algorithm(alg)!
	key.check_operation(.verify)!
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
			point := ec_uncompressed_point(params.coord_size, x, y)!
			pubkey := ecdsa.PublicKey.from_uncompressed_bytes(point, ecdsa.CurveOptions{
				nid: params.nid
			})!
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
			x := key.x or {
				d := key.d or { return error('cose: OKP key missing x and d') }
				if d.len != ed25519.seed_size {
					return error('cose: Ed25519 seed must be ${ed25519.seed_size} bytes, got ${d.len}')
				}
				ed25519.new_key_from_seed(d).public_key()
			}

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

// ec_uncompressed_point assembles the SEC 1 uncompressed point encoding
// `0x04 || X || Y` from COSE `x`/`y` coordinates, left-padding each one
// to the curve width. This is the form `crypto.ecdsa` accepts on both
// its OpenSSL and mbedTLS backends.
fn ec_uncompressed_point(coord_size int, x []u8, y []u8) ![]u8 {
	if x.len > coord_size || y.len > coord_size {
		return MalformedMessage{
			reason: 'EC2 coordinates exceed curve size'
		}
	}
	mut point := []u8{len: 1 + 2 * coord_size}
	point[0] = 0x04
	copy(mut point[1 + coord_size - x.len..1 + coord_size], x)
	copy(mut point[1 + 2 * coord_size - y.len..], y)
	return point
}
