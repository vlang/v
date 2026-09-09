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

struct RecipientAlgorithm {
	present bool
	value   i64
}

// Recipient is one entry of the `recipients` array of a COSE_Mac
// message. In "direct" mode the recipient carries only routing info
// (typically `kid` plus `alg = direct` in the unprotected header) and
// an empty `encrypted_key`.
pub struct Recipient {
pub mut:
	protected     Headers
	unprotected   Headers
	encrypted_key []u8
mut:
	// raw_protected holds this recipient's protected bstr exactly as it
	// was read by `decode`; see `protected_bytes_or`.
	raw_protected ?[]u8
}

// MacMessage is the V representation of a COSE_Mac message.
pub struct MacMessage {
pub mut:
	protected   Headers
	unprotected Headers
	payload     ?[]u8
	tag         []u8
	recipients  []Recipient
mut:
	// raw_protected holds the body protected bstr exactly as it was read
	// by `decode`; see `protected_bytes_or`.
	raw_protected ?[]u8
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
	if opts.recipients.len != 1 {
		return error('cose: direct-mode COSE_Mac requires exactly one recipient')
	}
	alg := opts.protected.algorithm or {
		return error('cose: COSE_Mac requires protected.algorithm to be set')
	}
	check_protected_headers(opts.protected, opts.unprotected)!

	signed_bytes := opts.detached_payload or { payload }
	body_protected := opts.protected.encode_protected()!
	tbm := mac_structure_mac(body_protected, opts.external_aad, signed_bytes)
	tag := compute_mac(alg, key, tbm)!

	// Build the on-wire recipients without mutating the caller's input
	// (struct copy + per-recipient header normalisation).
	mut recipients := []Recipient{cap: opts.recipients.len}
	for src in opts.recipients {
		check_protected_headers(src.protected, src.unprotected)!
		mut new_unprotected := src.unprotected
		has_direct := check_direct_recipient(src, false)!
		if !has_direct {
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
// checks it. Returns the payload bytes. An algorithm in the unprotected
// body bucket is accepted only when bound by `key.alg`.
pub fn verify_mac(message []u8, key Key, opts VerifyMacOptions) ![]u8 {
	msg := MacMessage.decode(message)!
	if msg.recipients.len != 1 {
		return MalformedMessage{
			reason: 'direct-mode COSE_Mac requires exactly one recipient'
		}
	}
	check_decoded_protected_unchanged(msg.raw_protected, msg.protected, 'Mac body')!
	check_protected_headers(msg.protected, msg.unprotected)!
	for i, r in msg.recipients {
		check_decoded_protected_unchanged(r.raw_protected, r.protected,
			'Mac recipient at index ${i}')!
		check_protected_headers(r.protected, r.unprotected)!
		check_direct_recipient(r, true)!
	}
	pl := payload_for_verification(msg.payload, opts.detached_payload,
		'VerifyMacOptions.detached_payload')!
	alg := verification_algorithm(msg.protected, msg.unprotected, key, 'Mac')!

	body_protected := msg.protected_bytes()!
	tbm := mac_structure_mac(body_protected, opts.external_aad, pl)
	mac_verify(alg, key, tbm, msg.tag)!
	return pl
}

fn header_algorithm(h Headers) !RecipientAlgorithm {
	mut result := RecipientAlgorithm{}
	if alg := h.algorithm {
		result = RecipientAlgorithm{
			present: true
			value:   i64(alg)
		}
	}
	for entry in h.extra_int_labels {
		if entry.label != label_alg {
			continue
		}
		if result.present {
			return MalformedMessage{
				reason: 'duplicate recipient algorithm header'
			}
		}
		result = RecipientAlgorithm{
			present: true
			value:   entry.value.as_int() or {
				return MalformedMessage{
					reason: 'recipient algorithm header is not an integer'
				}
			}
		}
	}
	return result
}

// check_direct_recipient validates the only recipient mode implemented by
// this module. It returns false for an absent algorithm when creation is
// allowed to auto-add `direct`.
fn check_direct_recipient(recipient Recipient, require_algorithm bool) !bool {
	if recipient.encrypted_key.len != 0 {
		return MalformedMessage{
			reason: 'direct COSE_Mac recipient must have an empty encrypted_key'
		}
	}
	if !recipient.protected.is_empty() {
		return MalformedMessage{
			reason: 'direct COSE_Mac recipient protected headers must be empty'
		}
	}
	if raw := recipient.raw_protected {
		if raw.len != 0 {
			return MalformedMessage{
				reason: 'direct COSE_Mac recipient protected headers must use an empty bstr'
			}
		}
	}
	unprotected_alg := header_algorithm(recipient.unprotected)!
	alg := if unprotected_alg.present {
		unprotected_alg.value
	} else {
		if require_algorithm {
			return MalformedMessage{
				reason: 'COSE_Mac recipient is missing alg = direct (-6)'
			}
		}
		return false
	}
	if alg != alg_direct {
		return MalformedMessage{
			reason: 'unsupported COSE_Mac recipient algorithm ${alg}; only direct (-6) is supported'
		}
	}
	return true
}

// protected_bytes returns the body protected bucket to feed into the
// MAC_structure: the bytes received on the wire for a decoded message,
// a canonical encoding of `protected` for one built in memory.
fn (m MacMessage) protected_bytes() ![]u8 {
	return protected_bytes_or(m.raw_protected, m.protected)!
}

// protected_bytes returns this recipient's protected bucket, following
// the same rule as `MacMessage.protected_bytes`.
fn (r Recipient) protected_bytes() ![]u8 {
	return protected_bytes_or(r.raw_protected, r.protected)!
}

// encode serialises the MacMessage. When `tagged` is true the output
// is wrapped in CBOR tag 97.
//
// For a message that came from `decode`, the protected bucket is
// written back exactly as it was received rather than re-serialised, so
// the bytes under the MAC tag survive a decode/encode cycle. Every
// other part of the message is re-encoded canonically. Mutating a decoded
// protected view is rejected until the message is produced again by `mac()`.
pub fn (m MacMessage) encode(tagged bool) ![]u8 {
	if m.recipients.len != 1 {
		return MalformedMessage{
			reason: 'direct-mode COSE_Mac requires exactly one recipient'
		}
	}
	check_decoded_protected_unchanged(m.raw_protected, m.protected, 'Mac body')!
	check_protected_headers(m.protected, m.unprotected)!
	for i, recipient in m.recipients {
		check_decoded_protected_unchanged(recipient.raw_protected, recipient.protected,
			'Mac recipient at index ${i}')!
		check_protected_headers(recipient.protected, recipient.unprotected)!
		check_direct_recipient(recipient, true)!
	}
	body_protected := m.protected_bytes()!

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
		rp := r.protected_bytes()!
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
	if header_count != -1 && header_count != 5 {
		return MalformedMessage{
			reason: 'Mac array must have 5 elements, got ${header_count}'
		}
	}

	raw_protected := u.unpack_bytes()!
	protected := parse_protected(raw_protected)!
	unprotected := parse_headers_value(u.unpack_value()!)!
	mut payload := ?[]u8(none)
	if u.peek_kind()! == .null_val {
		u.unpack_null()!
	} else {
		payload = u.unpack_bytes()!
	}
	tag := u.unpack_bytes()!

	recipients_count := u.unpack_array_header()!
	if recipients_count == 0 {
		return MalformedMessage{
			reason: 'Mac requires at least one recipient, got ${recipients_count}'
		}
	}
	if recipients_count > max_recipients {
		return MalformedMessage{
			reason: 'Mac claims ${recipients_count} recipients (over ${max_recipients}-entry sanity cap)'
		}
	}
	mut recipients := []Recipient{cap: if recipients_count == -1 { 1 } else { int(recipients_count) }}
	for recipients_count == -1 || recipients.len < recipients_count {
		if recipients_count == -1 && u.peek_break() {
			u.expect_break()!
			break
		}
		if recipients.len >= max_recipients {
			return MalformedMessage{
				reason: 'Mac has more than ${max_recipients} recipients'
			}
		}
		recipient_header_count := u.unpack_array_header()!
		if recipient_header_count != -1 && recipient_header_count != 3 {
			return MalformedMessage{
				reason: 'COSE_recipient array must have 3 elements'
			}
		}
		r_raw_protected := u.unpack_bytes()!
		r_protected := parse_protected(r_raw_protected)!
		r_unprotected := parse_headers_value(u.unpack_value()!)!
		ek := u.unpack_bytes()!
		if recipient_header_count == -1 {
			u.expect_break()!
		}
		recipients << Recipient{
			protected:     r_protected
			unprotected:   r_unprotected
			encrypted_key: ek
			raw_protected: r_raw_protected
		}
	}
	if recipients.len == 0 {
		return MalformedMessage{
			reason: 'Mac requires at least one recipient, got 0'
		}
	}
	if header_count == -1 {
		u.expect_break()!
	}
	if !u.done() {
		return MalformedMessage{
			reason: 'trailing bytes after Mac'
		}
	}
	return MacMessage{
		protected:     protected
		unprotected:   unprotected
		payload:       payload
		tag:           tag
		recipients:    recipients
		raw_protected: raw_protected
	}
}
