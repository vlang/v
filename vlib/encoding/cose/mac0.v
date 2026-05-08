// COSE_Mac0 — single-recipient MACed message — RFC 9052 §6.2.
module cose

import encoding.cbor

// Mac0Message is the V representation of a COSE_Mac0 message. The
// fields map 1:1 to the CBOR array slots defined by RFC 9052 §6.2.
pub struct Mac0Message {
pub mut:
	protected   Headers
	unprotected Headers
	// payload — `none` means a detached payload (the actual payload is
	// transmitted out of band).
	payload ?[]u8
	tag     []u8
}

// Mac0Options bundles the parameters that drive `cose.mac0`.
@[params]
pub struct Mac0Options {
pub:
	protected        Headers
	unprotected      Headers
	external_aad     []u8
	detached_payload ?[]u8
	untagged         bool
}

// VerifyMac0Options bundles inputs to `cose.verify_mac0`.
@[params]
pub struct VerifyMac0Options {
pub:
	external_aad     []u8
	detached_payload ?[]u8
}

// mac0 produces a tagged COSE_Mac0 message in one call.
pub fn mac0(payload []u8, key Key, opts Mac0Options) ![]u8 {
	mut msg := Mac0Message{
		protected:   opts.protected
		unprotected: opts.unprotected
		payload:     payload
	}
	pl := if dp := opts.detached_payload {
		dp
	} else {
		payload
	}
	msg.compute(key, pl, opts.external_aad)!
	if opts.detached_payload != none {
		msg.payload = none
	}
	return msg.encode(!opts.untagged)!
}

// verify_mac0 parses a (tagged or untagged) COSE_Mac0, verifies the tag
// against `key`, and returns the payload.
pub fn verify_mac0(message []u8, key Key, opts VerifyMac0Options) ![]u8 {
	msg := Mac0Message.decode(message)!
	pl := if dp := opts.detached_payload {
		dp
	} else {
		msg.payload or {
			return MalformedMessage{
				reason: 'detached payload requires VerifyMac0Options.detached_payload'
			}
		}
	}
	msg.verify(key, pl, opts.external_aad)!
	return pl
}

// compute computes the MAC tag and stores it in `tag`.
pub fn (mut m Mac0Message) compute(key Key, payload []u8, external_aad []u8) ! {
	alg := m.protected.algorithm or {
		return error('cose: Mac0Message.compute requires protected.algorithm to be set')
	}

	body_protected := m.protected.encode_protected()!
	tbm := mac_structure_mac0(body_protected, external_aad, payload)
	m.tag = compute_mac(alg, key, tbm)!
}

// verify recomputes the MAC tag and checks it against the stored one.
pub fn (m Mac0Message) verify(key Key, payload []u8, external_aad []u8) ! {
	check_critical(m.protected)!
	alg := m.protected.algorithm or {
		return MalformedMessage{
			reason: 'algorithm missing from protected header (RFC 9052 §3)'
		}
	}

	body_protected := m.protected.encode_protected()!
	tbm := mac_structure_mac0(body_protected, external_aad, payload)
	mac_verify(alg, key, tbm, m.tag)!
}

// encode serialises the message. When `tagged` is true the output is
// wrapped in CBOR tag 17 (RFC 9052 §2).
pub fn (m Mac0Message) encode(tagged bool) ![]u8 {
	body_protected := m.protected.encode_protected()!

	mut p := cbor.new_packer(cbor.EncodeOpts{ canonical: true })
	if tagged {
		p.pack_tag(tag_mac0)
	}
	p.pack_array_header(4)
	p.pack_bytes(body_protected)
	p.pack_value(m.unprotected.to_value())!
	if pl := m.payload {
		p.pack_bytes(pl)
	} else {
		p.pack_null()
	}
	p.pack_bytes(m.tag)
	return p.bytes()
}

// Mac0Message.decode parses a CBOR-encoded COSE_Mac0.
pub fn Mac0Message.decode(data []u8) !Mac0Message {
	mut u := cbor.new_unpacker(data, cbor.DecodeOpts{})
	if u.peek_kind()! == .tag_val {
		tag_no := u.unpack_tag()!
		if tag_no != tag_mac0 {
			return MalformedMessage{
				reason: 'expected tag ${tag_mac0} (Mac0), got ${tag_no}'
			}
		}
	}
	header_count := u.unpack_array_header()!
	if header_count != 4 {
		return MalformedMessage{
			reason: 'Mac0 array must have 4 elements, got ${header_count}'
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
	if !u.done() {
		return MalformedMessage{
			reason: 'trailing bytes after Mac0'
		}
	}
	return Mac0Message{
		protected:   protected
		unprotected: unprotected
		payload:     payload
		tag:         tag
	}
}
