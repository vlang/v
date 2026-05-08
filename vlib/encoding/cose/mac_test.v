// Tests for COSE_Mac — multi-recipient MACed messages (direct mode).
module cose

import encoding.base64

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
		assert err.msg().contains('at least one recipient')
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
