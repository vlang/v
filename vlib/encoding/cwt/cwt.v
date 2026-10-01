// CBOR Web Token (CWT) — RFC 8392 — module entry point.
//
// A CWT is a Claims Set serialised as CBOR and wrapped in a COSE
// message (typically COSE_Sign1 or COSE_Mac0). The optional CBOR tag
// 61 is used to distinguish a CWT from an arbitrary signed/MACed CBOR
// blob.
//
// This module is a thin layer on top of `encoding.cose`: the heavy
// lifting (signing, MACing, header handling) lives there. The two
// CWT-specific concerns are: (1) converting a `ClaimsSet` to/from the
// CBOR payload, and (2) handling the optional outer tag 61.
module cwt

import encoding.cbor
import encoding.cose

// SignOptions controls the signing of a CWT. The same fields as
// `cose.Sign1Options` plus a CWT-specific `tagged_cwt` flag controlling
// the outer tag 61 wrapper.
@[params]
pub struct SignOptions {
pub:
	protected    cose.Headers
	unprotected  cose.Headers
	external_aad []u8
	// untagged_cose disables the inner COSE tag 18 wrapper. The default
	// (tagged) is what every interop test in RFC 8392 uses.
	untagged_cose bool
	// tagged_cwt wraps the COSE message in CBOR tag 61. The default is
	// `true`. RFC 8392 §6 RECOMMENDS the tag for self-describing
	// payloads but allows omitting it when the context already
	// disambiguates.
	tagged_cwt bool = true
}

// VerifyOptions mirrors SignOptions for the verification side.
@[params]
pub struct VerifyOptions {
pub:
	external_aad []u8
}

// MacOptions controls the MACing of a CWT (Mac0 mode).
@[params]
pub struct MacOptions {
pub:
	protected     cose.Headers
	unprotected   cose.Headers
	external_aad  []u8
	untagged_cose bool
	tagged_cwt    bool = true
}

// VerifyMacOptions mirrors MacOptions for the verification side.
@[params]
pub struct VerifyMacOptions {
pub:
	external_aad []u8
}

// sign produces a signed CWT from `claims`. The resulting bytes are
// (optionally) tagged with CBOR tag 61 then carry a tagged COSE_Sign1
// whose payload is the encoded Claims Set.
pub fn sign(claims ClaimsSet, key cose.Key, opts SignOptions) ![]u8 {
	payload := claims.encode()!
	cose_message := cose.sign1(payload, key,
		protected:    opts.protected
		unprotected:  opts.unprotected
		external_aad: opts.external_aad
		untagged:     opts.untagged_cose
	)!
	return wrap_cwt(cose_message, opts.tagged_cwt)
}

// verify parses, unwraps and verifies a signed CWT, returning the
// claims set. The outer tag 61 is accepted-but-not-required.
pub fn verify(token []u8, key cose.Key, opts VerifyOptions) !ClaimsSet {
	cose_message := unwrap_cwt(token)!
	payload := cose.verify1(cose_message, key, external_aad: opts.external_aad)!
	return ClaimsSet.decode(payload)!
}

// mac produces a MACed CWT (Mac0 mode) from `claims`.
pub fn mac(claims ClaimsSet, key cose.Key, opts MacOptions) ![]u8 {
	payload := claims.encode()!
	cose_message := cose.mac0(payload, key,
		protected:    opts.protected
		unprotected:  opts.unprotected
		external_aad: opts.external_aad
		untagged:     opts.untagged_cose
	)!
	return wrap_cwt(cose_message, opts.tagged_cwt)
}

// verify_mac parses, unwraps and verifies a MACed CWT.
pub fn verify_mac(token []u8, key cose.Key, opts VerifyMacOptions) !ClaimsSet {
	cose_message := unwrap_cwt(token)!
	payload := cose.verify_mac0(cose_message, key, external_aad: opts.external_aad)!
	return ClaimsSet.decode(payload)!
}

// wrap_cwt prepends the CBOR tag 61 wrapper if `tagged` is true.
fn wrap_cwt(cose_message []u8, tagged bool) []u8 {
	if !tagged {
		return cose_message
	}
	mut p := cbor.new_packer(cbor.EncodeOpts{})
	p.pack_tag(tag_cwt)
	mut out := p.bytes()
	out << cose_message
	return out
}

// unwrap_cwt strips the optional outer tag 61 and returns the inner
// COSE message bytes. Tagged and untagged inputs are both accepted.
//
// The tag is parsed rather than matched byte-for-byte because CBOR permits
// non-preferred integer encodings such as `d9 003d` for tag 61.
fn unwrap_cwt(token []u8) ![]u8 {
	if token.len == 0 {
		return token.clone()
	}
	mut u := cbor.new_unpacker(token, cbor.DecodeOpts{})
	if u.peek_kind()! == .tag_val {
		tag := u.unpack_tag()!
		if tag == tag_cwt {
			consumed := token.len - u.remaining()
			return token[consumed..].clone()
		}
	}
	return token.clone()
}
