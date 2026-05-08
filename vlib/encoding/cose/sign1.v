// COSE_Sign1 — single-signer signed message — RFC 9052 §4.2.
module cose

import encoding.cbor

// Sign1Message is the V representation of a COSE_Sign1 message. The
// fields map 1:1 to the CBOR array slots defined by RFC 9052 §4.2.
//
// Use the `cose.sign1(...)` shorthand for the common case (sign +
// encode in one step) and the type's methods (`encode`, `sign`,
// `verify`) when finer control over the message is needed (e.g. setting
// custom headers, doing a detached payload, or signing with an external
// AAD).
pub struct Sign1Message {
pub mut:
	protected   Headers
	unprotected Headers
	// payload — `none` means a detached payload (the actual payload is
	// transmitted out of band). The signature is still computed over the
	// real payload bytes; supply them via `Sign1Options.detached_payload`
	// when signing or via `Verify1Options.detached_payload` when
	// verifying.
	payload   ?[]u8
	signature []u8
}

// Sign1Options bundles the parameters that drive `cose.sign1`. Only the
// algorithm in `protected.algorithm` is mandatory; everything else has
// safe defaults.
@[params]
pub struct Sign1Options {
pub:
	protected   Headers
	unprotected Headers
	// external_aad is data that is included in the signature
	// computation but not transmitted in the message. Both signer and
	// verifier must supply the same value.
	external_aad []u8
	// detached_payload, if set, overrides `payload` for the purpose of
	// the signature input. When detached, the encoded message will
	// contain a CBOR `nil` in the payload slot rather than the bytes.
	detached_payload ?[]u8
	// untagged emits the message without the surrounding tag 18 wrapper.
	untagged bool
}

// Verify1Options bundles inputs to `cose.verify1`. Defaults are the
// usual case (tagged message, attached payload, no external AAD).
@[params]
pub struct Verify1Options {
pub:
	external_aad     []u8
	detached_payload ?[]u8
}

// sign1 produces a tagged COSE_Sign1 message in one call. The
// algorithm in `opts.protected.algorithm` selects the signing routine
// and is integrity-protected by the signature.
pub fn sign1(payload []u8, key Key, opts Sign1Options) ![]u8 {
	signed_bytes := opts.detached_payload or { payload }
	mut msg := Sign1Message{
		protected:   opts.protected
		unprotected: opts.unprotected
		payload:     payload
	}
	msg.sign(key, signed_bytes, opts.external_aad)!
	if opts.detached_payload != none {
		msg.payload = none
	}
	return msg.encode(!opts.untagged)!
}

// verify1 parses a (tagged or untagged) COSE_Sign1, verifies the
// signature against `key`, and returns the payload. For detached
// payloads, the caller passes the bytes via `opts.detached_payload`.
pub fn verify1(message []u8, key Key, opts Verify1Options) ![]u8 {
	msg := Sign1Message.decode(message)!
	pl := if dp := opts.detached_payload {
		dp
	} else {
		msg.payload or {
			return MalformedMessage{
				reason: 'detached payload requires Verify1Options.detached_payload'
			}
		}
	}
	msg.verify(key, pl, opts.external_aad)!
	return pl
}

// sign computes the signature over the Sig_structure built from the
// message's protected headers and the supplied payload. The result is
// stored in `signature`. The message itself isn't mutated outside of
// the signature; the caller is expected to have set the algorithm in
// `protected` before calling.
pub fn (mut m Sign1Message) sign(key Key, payload []u8, external_aad []u8) ! {
	alg := m.protected.algorithm or {
		return error('cose: Sign1Message.sign requires protected.algorithm to be set')
	}

	body_protected := m.protected.encode_protected()!
	tbs := sig_structure_sign1(body_protected, external_aad, payload)
	m.signature = sign_with_key(alg, key, tbs)!
}

// verify recomputes the Sig_structure from the message's protected
// headers and the supplied payload, then checks the signature. The
// algorithm MUST be present in the protected headers per RFC 9052 §3
// — putting it in `unprotected` would let an attacker substitute it
// without invalidating the signature.
pub fn (m Sign1Message) verify(key Key, payload []u8, external_aad []u8) ! {
	check_critical(m.protected)!
	alg := m.protected.algorithm or {
		return MalformedMessage{
			reason: 'algorithm missing from protected header (RFC 9052 §3)'
		}
	}

	body_protected := m.protected.encode_protected()!
	tbs := sig_structure_sign1(body_protected, external_aad, payload)
	verify_with_key(alg, key, tbs, m.signature)!
}

// encode serialises the message. When `tagged` is true the output is
// wrapped in CBOR tag 18 (RFC 9052 §2).
pub fn (m Sign1Message) encode(tagged bool) ![]u8 {
	body_protected := m.protected.encode_protected()!

	mut p := cbor.new_packer(cbor.EncodeOpts{ canonical: true })
	if tagged {
		p.pack_tag(tag_sign1)
	}
	p.pack_array_header(4)
	p.pack_bytes(body_protected)
	p.pack_value(m.unprotected.to_value())!
	if pl := m.payload {
		p.pack_bytes(pl)
	} else {
		p.pack_null()
	}
	p.pack_bytes(m.signature)
	return p.bytes()
}

// Sign1Message.decode parses a CBOR-encoded COSE_Sign1. Both the tagged
// (tag 18) and untagged forms are accepted.
pub fn Sign1Message.decode(data []u8) !Sign1Message {
	mut u := cbor.new_unpacker(data, cbor.DecodeOpts{})
	if u.peek_kind()! == .tag_val {
		tag_no := u.unpack_tag()!
		if tag_no != tag_sign1 {
			return MalformedMessage{
				reason: 'expected tag ${tag_sign1} (Sign1), got ${tag_no}'
			}
		}
	}
	header_count := u.unpack_array_header()!
	if header_count != 4 {
		return MalformedMessage{
			reason: 'Sign1 array must have 4 elements, got ${header_count}'
		}
	}

	protected := parse_protected(u.unpack_bytes()!)!
	unprotected := parse_headers_value(u.unpack_value()!)!
	mut payload := ?[]u8(none)
	if u.peek_kind()! == .null_val {
		u.unpack_null()!
	} else {
		payload = u.unpack_bytes()!
	}
	signature := u.unpack_bytes()!
	if !u.done() {
		return MalformedMessage{
			reason: 'trailing bytes after Sign1'
		}
	}
	return Sign1Message{
		protected:   protected
		unprotected: unprotected
		payload:     payload
		signature:   signature
	}
}
