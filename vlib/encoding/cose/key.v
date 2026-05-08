// COSE_Key as defined by RFC 9052 §7 and the IANA "COSE Key Types" /
// "COSE Elliptic Curves" registries. EC2, OKP and Symmetric keys are
// covered (enough for ES256/384/512, EdDSA and HMAC); RSA support is
// gated on RSA primitives in `vlib/crypto`.
module cose

import encoding.cbor

// Common COSE_Key parameter labels (RFC 9052 §7.1, table 4).
const key_label_kty = i64(1)
const key_label_kid = i64(2)
const key_label_alg = i64(3)
const key_label_key_ops = i64(4)
const key_label_base_iv = i64(5)

// Type-specific key parameter labels (RFC 9053 §7.1 / §7.2 / §6.1).
const key_label_crv = i64(-1) // EC2, OKP
const key_label_x = i64(-2) // EC2 (x-coord), OKP (public key)
const key_label_y = i64(-3) // EC2 (y-coord, can be bool for compressed)
const key_label_d = i64(-4) // EC2, OKP (private key)
const key_label_k = i64(-1) // Symmetric

// KeyType identifies the cryptographic family of a COSE_Key (label 1).
// Values match the IANA "COSE Key Types" registry.
pub enum KeyType {
	okp       = 1 // Octet Key Pair (Ed25519, X25519…)
	ec2       = 2 // Elliptic Curve, two-coordinate
	rsa       = 3 // RSA — not yet supported by this module
	symmetric = 4
}

// Curve identifies an elliptic curve used by EC2 or OKP keys (label
// -1 of the type-specific parameters). Only the curves actually used
// by this module's algorithms are listed; others can still be parsed
// but are reported as unsupported when a key is converted to a
// signer/verifier.
pub enum Curve {
	p_256   = 1 // EC2, ES256
	p_384   = 2 // EC2, ES384
	p_521   = 3 // EC2, ES512 (note: 521-bit, not 512)
	ed25519 = 6 // OKP, EdDSA
}

// KeyOp restricts the operations a key may be used for (label 4).
// Values match the IANA "COSE Key Operation Values" registry.
pub enum KeyOp {
	sign        = 1
	verify      = 2
	encrypt     = 3
	decrypt     = 4
	wrap_key    = 5
	unwrap_key  = 6
	derive_key  = 7
	derive_bits = 8
	mac_create  = 9
	mac_verify  = 10
}

// Key is the V representation of a COSE_Key. Fields applicable to the
// `kty` are populated; others stay `none`. Use the typed constructors
// (`Key.ec2_*`, `Key.okp_*`, `Key.symmetric`) rather than building
// instances by hand — they enforce the invariants of each key type.
pub struct Key {
pub mut:
	kty     KeyType
	kid     ?[]u8
	alg     ?Algorithm
	key_ops []KeyOp
	base_iv ?[]u8

	// EC2 / OKP:
	crv ?Curve
	x   ?[]u8 // EC2 x-coordinate, or OKP public key
	y   ?[]u8 // EC2 y-coordinate
	d   ?[]u8 // private scalar (optional)

	// Symmetric:
	k ?[]u8
}

// Key.ec2_private builds an EC2 private key from raw coordinates and
// scalar. `x` and `y` are the public point components (big-endian, no
// leading 0x00 padding required), `d` is the private scalar.
pub fn Key.ec2_private(crv Curve, x []u8, y []u8, d []u8) Key {
	return Key{
		kty: .ec2
		crv: crv
		x:   x
		y:   y
		d:   d
	}
}

// Key.ec2_public builds an EC2 public key (no private scalar).
pub fn Key.ec2_public(crv Curve, x []u8, y []u8) Key {
	return Key{
		kty: .ec2
		crv: crv
		x:   x
		y:   y
	}
}

// Key.okp_private builds an OKP private key. For Ed25519, `x` is the
// 32-byte public key and `d` is the 32-byte private seed.
pub fn Key.okp_private(crv Curve, x []u8, d []u8) Key {
	return Key{
		kty: .okp
		crv: crv
		x:   x
		d:   d
	}
}

// Key.okp_public builds an OKP public key.
pub fn Key.okp_public(crv Curve, x []u8) Key {
	return Key{
		kty: .okp
		crv: crv
		x:   x
	}
}

// Key.symmetric builds a Symmetric key from raw key material.
pub fn Key.symmetric(k []u8) Key {
	return Key{
		kty: .symmetric
		k:   k
	}
}

// encode returns the canonical CBOR encoding of the COSE_Key.
pub fn (k Key) encode() ![]u8 {
	mut pairs := []cbor.MapPair{cap: 8}
	pairs << cbor.MapPair{
		key:   cbor.new_int(key_label_kty)
		value: cbor.new_int(i64(k.kty))
	}
	if kid := k.kid {
		pairs << cbor.MapPair{
			key:   cbor.new_int(key_label_kid)
			value: cbor.new_bytes(kid)
		}
	}
	if alg := k.alg {
		pairs << cbor.MapPair{
			key:   cbor.new_int(key_label_alg)
			value: cbor.new_int(i64(alg))
		}
	}
	if k.key_ops.len > 0 {
		mut ops_arr := cbor.Array{}
		for op in k.key_ops {
			ops_arr.elements << cbor.new_int(i64(op))
		}
		pairs << cbor.MapPair{
			key:   cbor.new_int(key_label_key_ops)
			value: ops_arr
		}
	}
	if biv := k.base_iv {
		pairs << cbor.MapPair{
			key:   cbor.new_int(key_label_base_iv)
			value: cbor.new_bytes(biv)
		}
	}

	match k.kty {
		.symmetric {
			km := k.k or { return error('cose: symmetric key missing k parameter') }
			pairs << cbor.MapPair{
				key:   cbor.new_int(key_label_k)
				value: cbor.new_bytes(km)
			}
		}
		.ec2, .okp {
			crv := k.crv or { return error('cose: ${k.kty} key missing crv parameter') }
			x := k.x or { return error('cose: ${k.kty} key missing x parameter') }
			pairs << cbor.MapPair{
				key:   cbor.new_int(key_label_crv)
				value: cbor.new_int(i64(crv))
			}
			pairs << cbor.MapPair{
				key:   cbor.new_int(key_label_x)
				value: cbor.new_bytes(x)
			}
			if k.kty == .ec2 {
				y := k.y or { return error('cose: EC2 key missing y parameter') }
				pairs << cbor.MapPair{
					key:   cbor.new_int(key_label_y)
					value: cbor.new_bytes(y)
				}
			}
			if d := k.d {
				pairs << cbor.MapPair{
					key:   cbor.new_int(key_label_d)
					value: cbor.new_bytes(d)
				}
			}
		}
		.rsa {
			return error('cose: RSA keys are not supported in this module version')
		}
	}

	return cbor.encode(cbor.Value(cbor.Map{ pairs: pairs }), cbor.EncodeOpts{
		canonical: true
	})!
}

// Key.decode parses a CBOR-encoded COSE_Key.
pub fn Key.decode(data []u8) !Key {
	v := cbor.decode[cbor.Value](data, cbor.DecodeOpts{})!
	m := if v is cbor.Map {
		v
	} else {
		return MalformedMessage{
			reason: 'COSE_Key is not a CBOR map'
		}
	}

	mut out := Key{}
	mut found_kty := false
	for pair in m.pairs {
		int_key := pair.key.as_int() or {
			// Text labels for keys are private use; we silently ignore
			// them on decode rather than failing — they don't affect
			// crypto operations.
			continue
		}
		match int_key {
			key_label_kty {
				code := pair.value.as_int() or {
					return MalformedMessage{
						reason: 'kty is not an integer'
					}
				}
				out.kty = match code {
					1 {
						KeyType.okp
					}
					2 {
						KeyType.ec2
					}
					3 {
						KeyType.rsa
					}
					4 {
						KeyType.symmetric
					}
					else {
						return MalformedMessage{
							reason: 'unknown kty ${code}'
						}
					}
				}

				found_kty = true
			}
			key_label_kid {
				out.kid = pair.value.as_bytes() or {
					return MalformedMessage{
						reason: 'kid is not bstr'
					}
				}
			}
			key_label_alg {
				code := pair.value.as_int() or {
					return MalformedMessage{
						reason: 'alg is not int'
					}
				}
				// Lenient: an unknown algorithm leaves `alg` as `none`
				// so the rest of the key (kid, public material, …)
				// remains usable. Symmetric with the Headers parser.
				if alg := algorithm_from_int(code) {
					out.alg = alg
				}
			}
			key_label_key_ops {
				items := pair.value.as_array() or {
					return MalformedMessage{
						reason: 'key_ops is not array'
					}
				}
				mut ops := []KeyOp{cap: items.len}
				for it in items {
					n := it.as_int() or {
						return MalformedMessage{
							reason: 'key_ops contains non-int'
						}
					}
					ops << match n {
						1 {
							KeyOp.sign
						}
						2 {
							KeyOp.verify
						}
						3 {
							KeyOp.encrypt
						}
						4 {
							KeyOp.decrypt
						}
						5 {
							KeyOp.wrap_key
						}
						6 {
							KeyOp.unwrap_key
						}
						7 {
							KeyOp.derive_key
						}
						8 {
							KeyOp.derive_bits
						}
						9 {
							KeyOp.mac_create
						}
						10 {
							KeyOp.mac_verify
						}
						else {
							return MalformedMessage{
								reason: 'unknown key_op ${n}'
							}
						}
					}
				}
				out.key_ops = ops
			}
			key_label_base_iv {
				out.base_iv = pair.value.as_bytes() or {
					return MalformedMessage{
						reason: 'base_iv is not bstr'
					}
				}
			}
			else {
				// Type-specific parameters are interpreted after kty is
				// known, in a second pass below.
			}
		}
	}
	if !found_kty {
		return MalformedMessage{
			reason: 'COSE_Key missing kty'
		}
	}

	for pair in m.pairs {
		int_key := pair.key.as_int() or { continue }
		match out.kty {
			.symmetric {
				if int_key == key_label_k {
					out.k = pair.value.as_bytes() or {
						return MalformedMessage{
							reason: 'k is not bstr'
						}
					}
				}
			}
			.ec2, .okp {
				match int_key {
					key_label_crv {
						code := pair.value.as_int() or {
							return MalformedMessage{
								reason: 'crv is not int'
							}
						}
						out.crv = match code {
							1 {
								Curve.p_256
							}
							2 {
								Curve.p_384
							}
							3 {
								Curve.p_521
							}
							6 {
								Curve.ed25519
							}
							else {
								return MalformedMessage{
									reason: 'unsupported crv ${code}'
								}
							}
						}
					}
					key_label_x {
						out.x = pair.value.as_bytes() or {
							return MalformedMessage{
								reason: 'x is not bstr'
							}
						}
					}
					key_label_y {
						// RFC 9053 §7.1.1 also allows a boolean `y` for
						// compressed points; that form is not supported.
						out.y = pair.value.as_bytes() or {
							return MalformedMessage{
								reason: 'y is not bstr (compressed points not supported)'
							}
						}
					}
					key_label_d {
						out.d = pair.value.as_bytes() or {
							return MalformedMessage{
								reason: 'd is not bstr'
							}
						}
					}
					else {}
				}
			}
			.rsa {}
		}
	}

	return out
}
