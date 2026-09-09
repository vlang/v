// Tests for COSE_Mac — multi-recipient MACed messages (direct mode).
module cose

import encoding.base64
import encoding.cbor

fn encode_mac_unchecked(m MacMessage) ![]u8 {
	mut p := cbor.new_packer(cbor.EncodeOpts{ canonical: true })
	p.pack_tag(tag_mac)
	p.pack_array_header(5)
	p.pack_bytes(m.protected_bytes()!)
	p.pack_value(m.unprotected.to_value())!
	if payload := m.payload {
		p.pack_bytes(payload)
	} else {
		p.pack_null()
	}
	p.pack_bytes(m.tag)
	p.pack_array_header(u64(m.recipients.len))
	for recipient in m.recipients {
		p.pack_array_header(3)
		p.pack_bytes(recipient.protected_bytes()!)
		p.pack_value(recipient.unprotected.to_value())!
		p.pack_bytes(recipient.encrypted_key)
	}
	return p.bytes()
}

fn test_mac_direct_recipient_roundtrip() {
	k := base64.url_decode('hJtXIZ2uSN5kbQfbtTNWbpdmhkV8FJG-Onbc6mxCcYg')
	key := Key.symmetric(k)
	mut hp := Headers{}
	hp.algorithm = .hmac_256_256
	mut hu_recip := Headers{}
	hu_recip.kid = 'our-secret'.bytes()
	recip := Recipient{
		unprotected: hu_recip
	}
	signed := mac('This is the content.'.bytes(), key,
		protected:  hp
		recipients: [
			recip,
		]
	)!
	msg := MacMessage.decode(signed)!
	assert msg.recipients.len == 1
	assert msg.tag.len == 32
	got := verify_mac(signed, key)!
	assert got == 'This is the content.'.bytes()
}

fn test_mac_rejects_no_recipients() {
	k := base64.url_decode('hJtXIZ2uSN5kbQfbtTNWbpdmhkV8FJG-Onbc6mxCcYg')
	key := Key.symmetric(k)
	mut hp := Headers{}
	hp.algorithm = .hmac_256_256
	if _ := mac('payload'.bytes(), key, protected: hp) {
		assert false, 'must reject zero recipients'
	} else {
		assert err.msg().contains('exactly one recipient')
	}
}

fn test_mac_direct_mode_rejects_multiple_recipients() {
	key := Key.symmetric([]u8{len: 32, init: 1})
	mut hp := Headers{}
	hp.algorithm = .hmac_256_256
	if _ := mac('payload'.bytes(), key,
		protected: hp
		recipients: [
			Recipient{},
			Recipient{},
		]
	) {
		assert false, 'direct mode must reject multiple recipients on creation'
	} else {
		assert err.msg().contains('exactly one recipient')
	}

	signed := mac('payload'.bytes(), key, protected: hp, recipients: [Recipient{}])!
	mut decoded := MacMessage.decode(signed)!
	decoded.recipients << decoded.recipients[0]
	if _ := decoded.encode(true) {
		assert false, 'low-level encoding must reject multiple direct recipients'
	} else {
		assert err.msg().contains('exactly one recipient')
	}
	if _ := verify_mac(encode_mac_unchecked(decoded)!, key) {
		assert false, 'direct mode must reject multiple recipients on verification'
	} else {
		assert err.msg().contains('exactly one recipient')
	}
}

fn test_mac_decode_rejects_huge_recipients_count() {
	// COSE_Mac with a recipients-array length declared as 4 billion;
	// the sanity cap must reject it without allocating that much memory.
	mut bad := []u8{}
	bad << 0xD8 // tag
	bad << 0x61 // tag 97
	bad << 0x85 // array(5)
	bad << 0x40 // bstr(0) — protected
	bad << 0xA0 // map(0) — unprotected
	bad << 0x40 // bstr(0) — payload
	bad << 0x40 // bstr(0) — tag
	// recipients array header: 0x9A 0xFFFFFFFF
	bad << 0x9A
	bad << 0xFF
	bad << 0xFF
	bad << 0xFF
	bad << 0xFF
	if _ := MacMessage.decode(bad) {
		assert false, 'must reject huge recipients count'
	} else {
		assert err is MalformedMessage
	}
}

fn test_mac_recipient_alg_direct_auto_added() {
	k := base64.url_decode('hJtXIZ2uSN5kbQfbtTNWbpdmhkV8FJG-Onbc6mxCcYg')
	key := Key.symmetric(k)
	mut hp := Headers{}
	hp.algorithm = .hmac_256_256
	mut hu_recip := Headers{}
	hu_recip.kid = 'r1'.bytes()
	recip := Recipient{
		unprotected: hu_recip
	}
	signed := mac('p'.bytes(), key, protected: hp, recipients: [recip])!
	msg := MacMessage.decode(signed)!
	// The recipient's unprotected header must carry alg = direct (-6).
	first := msg.recipients[0]
	mut found_direct := false
	for e in first.unprotected.extra_int_labels {
		if e.label == 1 {
			if v := e.value.as_int() {
				if v == -6 {
					found_direct = true
				}
			}
		}
	}
	assert found_direct
}

fn test_mac_rejects_non_direct_recipient_algorithm() {
	key := Key.symmetric([]u8{len: 32, init: 1})
	mut hp := Headers{}
	hp.algorithm = .hmac_256_256
	for recipient in [
		Recipient{
			protected: Headers{
				algorithm: .es256
			}
		},
		Recipient{
			unprotected: Headers{
				algorithm: .es256
			}
		},
	] {
		if _ := mac('payload'.bytes(), key, protected: hp, recipients: [recipient]) {
			assert false, 'only direct recipient algorithms are supported'
		} else {
			assert err.msg().contains('only direct')
		}
	}
}

fn test_verify_mac_requires_direct_recipient_algorithm() {
	key := Key.symmetric([]u8{len: 32, init: 1})
	mut hp := Headers{}
	hp.algorithm = .hmac_256_256
	signed := mac('payload'.bytes(), key, protected: hp, recipients: [Recipient{}])!
	mut msg := MacMessage.decode(signed)!
	msg.recipients[0].unprotected = Headers{}
	if _ := verify_mac(msg.encode(true)!, key) {
		assert false, 'verification must require alg = direct for every recipient'
	} else {
		assert err.msg().contains('missing alg = direct')
	}
}

fn test_mac_decodes_indefinite_length_arrays() {
	mut p := cbor.new_packer(cbor.EncodeOpts{})
	p.pack_array_indef()!
	p.pack_bytes([]u8{})
	p.pack_value(Headers{}.to_value())!
	p.pack_null()
	p.pack_bytes([u8(1)])
	p.pack_array_indef()!
	p.pack_array_indef()!
	p.pack_bytes([]u8{})
	p.pack_value(Headers{}.to_value())!
	p.pack_bytes([]u8{})
	p.pack_break()!
	p.pack_break()!
	p.pack_break()!
	msg := MacMessage.decode(p.bytes())!
	assert msg.payload == none
	assert msg.tag == [u8(1)]
	assert msg.recipients.len == 1
}
