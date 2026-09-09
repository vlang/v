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

mut:
	// raw_algorithm preserves an unsupported label-3 value read from a
	// COSE_Key.  It prevents a decoded constrained key from silently being
	// treated as unconstrained while still allowing it to round-trip.
	raw_algorithm      ?i64
	raw_algorithm_text ?string
	// raw_curve preserves a valid but unsupported integer curve identifier.
	raw_curve ?i64
	// raw_key_ops preserves valid text and future/private integer operations.
	raw_key_ops []cbor.Value
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
	} else if raw_algorithm := k.raw_algorithm {
		pairs << cbor.MapPair{
			key:   cbor.new_int(key_label_alg)
			value: cbor.new_int(raw_algorithm)
		}
	} else if raw_algorithm_text := k.raw_algorithm_text {
		pairs << cbor.MapPair{
			key:   cbor.new_int(key_label_alg)
			value: cbor.new_text(raw_algorithm_text)
		}
	}
	if k.key_ops.len > 0 || k.raw_key_ops.len > 0 {
		mut ops_arr := cbor.Array{}
		mut seen_int_ops := map[i64]bool{}
		mut seen_text_ops := map[string]bool{}
		for op in k.key_ops {
			code := i64(op)
			if code in seen_int_ops {
				return MalformedMessage{
					reason: 'duplicate key_ops entry ${code}'
				}
			}
			seen_int_ops[code] = true
			ops_arr.elements << cbor.new_int(code)
		}
		for op in k.raw_key_ops {
			if code := op.as_int() {
				if code in seen_int_ops {
					return MalformedMessage{
						reason: 'duplicate key_ops entry ${code}'
					}
				}
				seen_int_ops[code] = true
			} else if name := op.as_string() {
				if name in seen_text_ops {
					return MalformedMessage{
						reason: 'duplicate key_ops entry "${name}"'
					}
				}
				seen_text_ops[name] = true
			} else {
				return MalformedMessage{
					reason: 'key_ops entry is neither int nor tstr'
				}
			}
			ops_arr.elements << op
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
			curve_code := if crv := k.crv {
				i64(crv)
			} else if raw_curve := k.raw_curve {
				raw_curve
			} else {
				return error('cose: ${k.kty} key missing crv parameter')
			}
			pairs << cbor.MapPair{
				key:   cbor.new_int(key_label_crv)
				value: cbor.new_int(curve_code)
			}
			if x := k.x {
				pairs << cbor.MapPair{
					key:   cbor.new_int(key_label_x)
					value: cbor.new_bytes(x)
				}
			} else if k.kty == .ec2 || k.d == none {
				return error('cose: ${k.kty} key missing x parameter')
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
	v := cbor.decode[cbor.Value](data, cbor.DecodeOpts{ deny_duplicate_keys: true })!
	m := if v is cbor.Map {
		v
	} else {
		return MalformedMessage{
			reason: 'COSE_Key is not a CBOR map'
		}
	}

	mut out := Key{}
	mut found_kty := false
	mut seen_int_labels := map[i64]bool{}
	mut seen_text_labels := map[string]bool{}
	for pair in m.pairs {
		if text_key := pair.key.as_string() {
			if text_key in seen_text_labels {
				return MalformedMessage{
					reason: 'duplicate COSE_Key label "${text_key}"'
				}
			}
			seen_text_labels[text_key] = true
			continue
		}
		int_key := pair.key.as_int() or {
			// Other private-use key types do not affect crypto operations.
			continue
		}
		if int_key in seen_int_labels {
			return MalformedMessage{
				reason: 'duplicate COSE_Key label ${int_key}'
			}
		}
		seen_int_labels[int_key] = true
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
				if code := pair.value.as_int() {
					if alg := algorithm_from_int(code) {
						out.alg = alg
					} else {
						out.raw_algorithm = code
					}
				} else if name := pair.value.as_string() {
					out.raw_algorithm_text = name
				} else {
					return MalformedMessage{
						reason: 'alg is neither int nor tstr'
					}
				}
			}
			key_label_key_ops {
				items := pair.value.as_array() or {
					return MalformedMessage{
						reason: 'key_ops is not array'
					}
				}
				if items.len == 0 {
					return MalformedMessage{
						reason: 'key_ops array must not be empty'
					}
				}
				mut ops := []KeyOp{cap: items.len}
				mut raw_ops := []cbor.Value{}
				mut seen_int_ops := map[i64]bool{}
				mut seen_text_ops := map[string]bool{}
				for it in items {
					if n := it.as_int() {
						if n in seen_int_ops {
							return MalformedMessage{
								reason: 'duplicate key_ops entry ${n}'
							}
						}
						seen_int_ops[n] = true
						match n {
							1 { ops << .sign }
							2 { ops << .verify }
							3 { ops << .encrypt }
							4 { ops << .decrypt }
							5 { ops << .wrap_key }
							6 { ops << .unwrap_key }
							7 { ops << .derive_key }
							8 { ops << .derive_bits }
							9 { ops << .mac_create }
							10 { ops << .mac_verify }
							else { raw_ops << it }
						}
					} else if name := it.as_string() {
						if name in seen_text_ops {
							return MalformedMessage{
								reason: 'duplicate key_ops entry "${name}"'
							}
						}
						seen_text_ops[name] = true
						raw_ops << it
					} else {
						return MalformedMessage{
							reason: 'key_ops entry is neither int nor tstr'
						}
					}
				}
				out.key_ops = ops
				out.raw_key_ops = raw_ops
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
						match code {
							1 {
								out.crv = .p_256
							}
							2 {
								out.crv = .p_384
							}
							3 {
								out.crv = .p_521
							}
							6 {
								out.crv = .ed25519
							}
							else {
								out.raw_curve = code
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

// check_algorithm allows keys without an algorithm restriction, enforces a
// supported restriction, and rejects unsupported restrictions preserved by
// Key.decode.
fn (k Key) check_algorithm(alg Algorithm) ! {
	if key_alg := k.alg {
		if key_alg != alg {
			return AlgorithmMismatch{
				expected: key_alg
				got:      alg
			}
		}
	} else if raw_algorithm := k.raw_algorithm {
		return error('cose: key constrains use to unsupported algorithm ${raw_algorithm}')
	} else if raw_algorithm_text := k.raw_algorithm_text {
		return error('cose: key constrains use to unsupported algorithm "${raw_algorithm_text}"')
	}
}

// check_operation enforces a non-empty key_ops restriction.
fn (k Key) check_operation(required KeyOp) ! {
	if (k.key_ops.len > 0 || k.raw_key_ops.len > 0) && required !in k.key_ops {
		return error('cose: key_ops does not permit ${required}')
	}
}
