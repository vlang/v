// COSE_Sign — multi-signer signed message — RFC 9052 §4.1.
//
// COSE_Sign carries one payload signed by N signers, each with its own
// per-signer protected/unprotected headers and signature. Each signer
// can use a different algorithm. Verification is per-signer: the body
// is considered authentic if at least one signature verifies (the
// caller decides which signers it trusts).
module cose

import encoding.cbor

// max_signers caps the `signatures` array size a decoder will accept.
// RFC 9052 places no explicit bound, but real-world COSE_Sign messages
// carry one or a handful of signers; anything wildly larger is almost
// certainly a malformed or hostile input trying to OOM the parser.
const max_signers = 256

// Signature is one entry of the `signatures` array of a COSE_Sign
// message. The per-signer protected header MUST contain the algorithm.
pub struct Signature {
pub mut:
	protected   Headers
	unprotected Headers
	signature   []u8
}

// SignMessage is the V representation of a COSE_Sign message.
pub struct SignMessage {
pub mut:
	protected   Headers
	unprotected Headers
	payload     ?[]u8
	signatures  []Signature
}

// Signer bundles a signing key with the per-signer headers that go
// into the COSE_Sign message. Use a separate `Signer` per identity
// when producing a multi-signer message.
pub struct Signer {
pub:
	key         Key
	protected   Headers
	unprotected Headers
}

// SignOptions bundles inputs to `cose.sign`.
@[params]
pub struct SignOptions {
pub:
	protected        Headers
	unprotected      Headers
	external_aad     []u8
	detached_payload ?[]u8
	untagged         bool
}

// sign produces a tagged COSE_Sign message in one call. Each `Signer`
// in `signers` adds one entry to the `signatures` array; their
// per-signer protected headers must include the algorithm to use.
pub fn sign(payload []u8, signers []Signer, opts SignOptions) ![]u8 {
	if signers.len == 0 {
		return error('cose: COSE_Sign requires at least one signer')
	}
	signed_bytes := opts.detached_payload or { payload }
	body_protected := opts.protected.encode_protected()!

	mut entries := []Signature{cap: signers.len}
	for s in signers {
		alg := s.protected.algorithm or {
			return error('cose: each signer must declare an algorithm in protected headers')
		}

		sign_protected := s.protected.encode_protected()!
		tbs := sig_structure_sign(body_protected, sign_protected, opts.external_aad, signed_bytes)
		sig := sign_with_key(alg, s.key, tbs)!
		entries << Signature{
			protected:   s.protected
			unprotected: s.unprotected
			signature:   sig
		}
	}

	mut msg := SignMessage{
		protected:   opts.protected
		unprotected: opts.unprotected
		payload:     payload
		signatures:  entries
	}
	if opts.detached_payload != none {
		msg.payload = none
	}
	return msg.encode(!opts.untagged)!
}

// VerifySignOptions bundles inputs to per-signer verification.
@[params]
pub struct VerifySignOptions {
pub:
	external_aad     []u8
	detached_payload ?[]u8
}

// verify checks the signature at `signer_index` of the message against
// `key`. By default the payload is taken from `m.payload`; pass
// `opts.detached_payload` for the detached case. The per-signer
// algorithm MUST be in that signer's protected header (RFC 9052 §3).
pub fn (m SignMessage) verify(signer_index int, key Key, opts VerifySignOptions) ! {
	if signer_index < 0 || signer_index >= m.signatures.len {
		return error('cose: signer index ${signer_index} out of range (have ${m.signatures.len})')
	}
	entry := m.signatures[signer_index]
	check_critical(m.protected)!
	check_critical(entry.protected)!
	alg := entry.protected.algorithm or {
		return MalformedMessage{
			reason: 'signer at index ${signer_index} missing algorithm in protected header'
		}
	}

	pl := if dp := opts.detached_payload {
		dp
	} else {
		m.payload or {
			return MalformedMessage{
				reason: 'detached payload requires VerifySignOptions.detached_payload'
			}
		}
	}
	body_protected := m.protected.encode_protected()!
	sign_protected := entry.protected.encode_protected()!
	tbs := sig_structure_sign(body_protected, sign_protected, opts.external_aad, pl)
	verify_with_key(alg, key, tbs, entry.signature)!
}

// encode serialises the SignMessage. When `tagged` is true the output
// is wrapped in CBOR tag 98.
pub fn (m SignMessage) encode(tagged bool) ![]u8 {
	body_protected := m.protected.encode_protected()!

	mut p := cbor.new_packer(cbor.EncodeOpts{ canonical: true })
	if tagged {
		p.pack_tag(tag_sign)
	}
	p.pack_array_header(4)
	p.pack_bytes(body_protected)
	p.pack_value(m.unprotected.to_value())!
	if pl := m.payload {
		p.pack_bytes(pl)
	} else {
		p.pack_null()
	}
	p.pack_array_header(u64(m.signatures.len))
	for entry in m.signatures {
		sp := entry.protected.encode_protected()!
		p.pack_array_header(3)
		p.pack_bytes(sp)
		p.pack_value(entry.unprotected.to_value())!
		p.pack_bytes(entry.signature)
	}
	return p.bytes()
}

// SignMessage.decode parses a CBOR-encoded COSE_Sign.
pub fn SignMessage.decode(data []u8) !SignMessage {
	mut u := cbor.new_unpacker(data, cbor.DecodeOpts{})
	if u.peek_kind()! == .tag_val {
		tag_no := u.unpack_tag()!
		if tag_no != tag_sign {
			return MalformedMessage{
				reason: 'expected tag ${tag_sign} (Sign), got ${tag_no}'
			}
		}
	}
	header_count := u.unpack_array_header()!
	if header_count != 4 {
		return MalformedMessage{
			reason: 'Sign array must have 4 elements, got ${header_count}'
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

	signers_count := u.unpack_array_header()!
	if signers_count <= 0 {
		return MalformedMessage{
			reason: 'Sign requires at least one signature, got ${signers_count}'
		}
	}
	if signers_count > max_signers {
		return MalformedMessage{
			reason: 'Sign claims ${signers_count} signatures (over ${max_signers}-entry sanity cap)'
		}
	}
	mut signatures := []Signature{cap: int(signers_count)}
	for _ in 0 .. signers_count {
		if u.unpack_array_header()! != 3 {
			return MalformedMessage{
				reason: 'COSE_Signature array must have 3 elements'
			}
		}
		// Bind the three fields to locals so the read order from the
		// unpacker stays explicit (struct-literal evaluation order is
		// source order in V today, but we don't want to rely on that).
		s_protected := parse_protected(u.unpack_bytes()!)!
		s_unprotected := parse_headers_value(u.unpack_value()!)!
		sig := u.unpack_bytes()!
		signatures << Signature{
			protected:   s_protected
			unprotected: s_unprotected
			signature:   sig
		}
	}
	if !u.done() {
		return MalformedMessage{
			reason: 'trailing bytes after Sign'
		}
	}
	return SignMessage{
		protected:   protected
		unprotected: unprotected
		payload:     payload
		signatures:  signatures
	}
}
