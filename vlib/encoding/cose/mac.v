// COSE_Mac — multi-recipient MACed message — RFC 9052 §6.1.
//
// COSE_Mac carries a single MAC tag computed over the body, plus an
// array of per-recipient envelopes that describe how each recipient
// derives or wraps the MAC key. Only the "direct" recipient mode
// (RFC 9053 §6.1.1) is supported: each recipient is identified by
// `kid` and is assumed to share the symmetric key out of band, so the
// `encrypted_key` slot of every recipient is empty. The other
// recipient modes (key wrap, key derivation) require AEAD primitives
// that are not yet wired up.
module cose

import encoding.cbor

// alg_direct is the COSE algorithm value for "direct" key derivation
// (RFC 9053 §6.1.1, IANA "COSE Algorithms" registry).
const alg_direct = i64(-6)

// max_recipients caps the `recipients` array size on decode. See the
// rationale on `max_signers` in sign.v.
const max_recipients = 256

// Recipient is one entry of the `recipients` array of a COSE_Mac
// message. In "direct" mode the recipient carries only routing info
// (typically `kid` plus `alg = direct` in the unprotected header) and
// an empty `encrypted_key`.
pub struct Recipient {
pub mut:
	protected     Headers
	unprotected   Headers
	encrypted_key []u8
}

// MacMessage is the V representation of a COSE_Mac message.
pub struct MacMessage {
pub mut:
	protected   Headers
	unprotected Headers
	payload     ?[]u8
	tag         []u8
	recipients  []Recipient
}

// MacOptions bundles inputs to `cose.mac`.
@[params]
pub struct MacOptions {
pub:
	protected        Headers
	unprotected      Headers
	external_aad     []u8
	detached_payload ?[]u8
	untagged         bool
	// recipients: at least one entry. Each entry SHOULD set its
	// unprotected `kid` so that the receiver can pick the right shared
	// key. The `alg = direct (-6)` parameter is auto-added if absent.
	recipients []Recipient
}

// VerifyMacOptions bundles inputs to `cose.verify_mac`.
@[params]
pub struct VerifyMacOptions {
pub:
	external_aad     []u8
	detached_payload ?[]u8
}

// mac produces a tagged COSE_Mac message. The MAC tag is computed
// once, over the body — recipients are descriptive routing only in
// "direct" mode. The body algorithm in `opts.protected.algorithm`
// drives the MAC computation and the symmetric `key` is the shared
// secret named by the recipients' `kid`.
pub fn mac(payload []u8, key Key, opts MacOptions) ![]u8 {
	if opts.recipients.len == 0 {
		return error('cose: COSE_Mac requires at least one recipient')
	}
	alg := opts.protected.algorithm or {
		return error('cose: COSE_Mac requires protected.algorithm to be set')
	}

	signed_bytes := opts.detached_payload or { payload }
	body_protected := opts.protected.encode_protected()!
	tbm := mac_structure_mac(body_protected, opts.external_aad, signed_bytes)
	tag := compute_mac(alg, key, tbm)!

	// Build the on-wire recipients without mutating the caller's input
	// (struct copy + per-recipient header normalisation).
	mut recipients := []Recipient{cap: opts.recipients.len}
	for src in opts.recipients {
		mut new_unprotected := src.unprotected
		if new_unprotected.algorithm == none && !has_int_label(new_unprotected, label_alg) {
			// Re-allocate the slice so we don't mutate the caller's
			// Headers if it shares its backing array.
			mut extras := []HeaderEntry{cap: new_unprotected.extra_int_labels.len + 1}
			extras << new_unprotected.extra_int_labels
			extras << HeaderEntry{
				label: label_alg
				value: cbor.new_int(alg_direct)
			}
			new_unprotected.extra_int_labels = extras
		}
		recipients << Recipient{
			protected:     src.protected
			unprotected:   new_unprotected
			encrypted_key: []u8{} // direct mode → empty bstr
		}
	}

	mut msg := MacMessage{
		protected:   opts.protected
		unprotected: opts.unprotected
		payload:     payload
		tag:         tag
		recipients:  recipients
	}
	if opts.detached_payload != none {
		msg.payload = none
	}
	return msg.encode(!opts.untagged)!
}

// verify_mac parses a COSE_Mac, recomputes the MAC tag with `key` and
// checks it. Returns the payload bytes. The algorithm MUST be present
// in the protected headers per RFC 9052 §3.
pub fn verify_mac(message []u8, key Key, opts VerifyMacOptions) ![]u8 {
	msg := MacMessage.decode(message)!
	check_critical(msg.protected)!
	pl := if dp := opts.detached_payload {
		dp
	} else {
		msg.payload or {
			return MalformedMessage{
				reason: 'detached payload requires VerifyMacOptions.detached_payload'
			}
		}
	}
	alg := msg.protected.algorithm or {
		return MalformedMessage{
			reason: 'algorithm missing from protected header (RFC 9052 §3)'
		}
	}

	body_protected := msg.protected.encode_protected()!
	tbm := mac_structure_mac(body_protected, opts.external_aad, pl)
	mac_verify(alg, key, tbm, msg.tag)!
	return pl
}

// encode serialises the MacMessage. When `tagged` is true the output
// is wrapped in CBOR tag 97.
pub fn (m MacMessage) encode(tagged bool) ![]u8 {
	body_protected := m.protected.encode_protected()!

	mut p := cbor.new_packer(cbor.EncodeOpts{ canonical: true })
	if tagged {
		p.pack_tag(tag_mac)
	}
	p.pack_array_header(5)
	p.pack_bytes(body_protected)
	p.pack_value(m.unprotected.to_value())!
	if pl := m.payload {
		p.pack_bytes(pl)
	} else {
		p.pack_null()
	}
	p.pack_bytes(m.tag)
	p.pack_array_header(u64(m.recipients.len))
	for r in m.recipients {
		rp := r.protected.encode_protected()!
		p.pack_array_header(3)
		p.pack_bytes(rp)
		p.pack_value(r.unprotected.to_value())!
		p.pack_bytes(r.encrypted_key)
	}
	return p.bytes()
}

// MacMessage.decode parses a CBOR-encoded COSE_Mac.
pub fn MacMessage.decode(data []u8) !MacMessage {
	mut u := cbor.new_unpacker(data, cbor.DecodeOpts{})
	if u.peek_kind()! == .tag_val {
		tag_no := u.unpack_tag()!
		if tag_no != tag_mac {
			return MalformedMessage{
				reason: 'expected tag ${tag_mac} (Mac), got ${tag_no}'
			}
		}
	}
	header_count := u.unpack_array_header()!
	if header_count != 5 {
		return MalformedMessage{
			reason: 'Mac array must have 5 elements, got ${header_count}'
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
	tag := u.unpack_bytes()!

	recipients_count := u.unpack_array_header()!
	if recipients_count <= 0 {
		return MalformedMessage{
			reason: 'Mac requires at least one recipient, got ${recipients_count}'
		}
	}
	if recipients_count > max_recipients {
		return MalformedMessage{
			reason: 'Mac claims ${recipients_count} recipients (over ${max_recipients}-entry sanity cap)'
		}
	}
	mut recipients := []Recipient{cap: int(recipients_count)}
	for _ in 0 .. recipients_count {
		if u.unpack_array_header()! != 3 {
			return MalformedMessage{
				reason: 'COSE_recipient array must have 3 elements'
			}
		}
		r_protected := parse_protected(u.unpack_bytes()!)!
		r_unprotected := parse_headers_value(u.unpack_value()!)!
		ek := u.unpack_bytes()!
		recipients << Recipient{
			protected:     r_protected
			unprotected:   r_unprotected
			encrypted_key: ek
		}
	}
	if !u.done() {
		return MalformedMessage{
			reason: 'trailing bytes after Mac'
		}
	}
	return MacMessage{
		protected:   protected
		unprotected: unprotected
		payload:     payload
		tag:         tag
		recipients:  recipients
	}
}

// has_int_label reports whether `h` already declares the given integer
// label, either via a typed well-known field or via `extra_int_labels`.
// Used internally to avoid double-setting `alg = direct` on
// recipients that the caller has already configured.
fn has_int_label(h Headers, label i64) bool {
	if label == label_alg && h.algorithm != none {
		return true
	}
	for e in h.extra_int_labels {
		if e.label == label {
			return true
		}
	}
	return false
}
