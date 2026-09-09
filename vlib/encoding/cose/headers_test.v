// Tests for the Headers struct: encoding, decoding, canonical map order,
// extra labels, content-type variants and the "empty protected = empty
// bstr" rule of RFC 9052 §3.
module cose

import encoding.cbor
import encoding.hex

fn test_empty_protected_is_zero_length() {
	h := Headers{}
	assert h.is_empty()
	enc := h.encode_protected()!
	assert enc.len == 0
}

fn test_alg_only_protected_matches_es256_vector() {
	mut h := Headers{}
	h.algorithm = .es256
	enc := h.encode_protected()!
	// canonical CBOR map with one (1 -> -7) pair: A1 01 26
	assert enc == hex.decode('A10126')!
}

fn test_alg_and_ctyp_protected_matches_ecdsa01_vector() {
	mut h := Headers{}
	h.algorithm = .es256
	h.content_type_int = u64(0)
	enc := h.encode_protected()!
	// A2 01 26 03 00
	assert enc == hex.decode('A201260300')!
}

fn test_canonical_sorts_keys() {
	// Add labels out of order; canonical encoding must reorder them.
	mut h := Headers{}
	h.kid = 'k'.bytes()
	h.algorithm = .es256
	enc := h.encode_protected()!
	// kid (4) must come AFTER alg (1) in canonical order.
	// A2 01 26 04 41 6B
	assert enc == hex.decode('A201260441'.to_upper() + '6B')!
}

fn test_roundtrip_kid_and_alg() {
	mut h := Headers{}
	h.algorithm = .es256
	h.kid = 'my-key'.bytes()
	enc := h.encode_protected()!
	parsed := parse_protected(enc)!
	assert parsed.algorithm == ?Algorithm(.es256)
	assert (parsed.kid or { []u8{} }) == 'my-key'.bytes()
}

fn test_unknown_int_label_preserved_on_roundtrip() {
	mut h := Headers{}
	h.algorithm = .es256
	h.extra_int_labels << HeaderEntry{
		label: 99
		value: cbor.new_text('hello')
	}
	enc := h.encode_protected()!
	parsed := parse_protected(enc)!
	assert parsed.algorithm == ?Algorithm(.es256)
	assert parsed.extra_int_labels.len == 1
	assert parsed.extra_int_labels[0].label == 99
	assert (parsed.extra_int_labels[0].value.as_string() or { '' }) == 'hello'
}

fn test_text_label_preserved_on_roundtrip() {
	mut h := Headers{}
	h.extra_text_labels << TextHeaderEntry{
		label: 'app-id'
		value: cbor.new_int(42)
	}
	enc := h.encode_protected()!
	parsed := parse_protected(enc)!
	assert parsed.extra_text_labels.len == 1
	assert parsed.extra_text_labels[0].label == 'app-id'
	assert (parsed.extra_text_labels[0].value.as_int() or { 0 }) == 42
}

fn test_content_type_text_form_roundtrips() {
	mut h := Headers{}
	h.content_type_text = 'application/cbor'
	enc := h.encode_protected()!
	parsed := parse_protected(enc)!
	assert (parsed.content_type_text or { '' }) == 'application/cbor'
	assert parsed.content_type_int == none
}

fn test_critical_array_roundtrips() {
	mut h := Headers{}
	h.algorithm = .es256
	h.critical = [i64(99), i64(100)]
	enc := h.encode_protected()!
	parsed := parse_protected(enc)!
	assert parsed.critical == [i64(99), i64(100)]
}

fn test_unknown_alg_falls_back_to_extra_label() {
	// 1 -> -1000 is a valid CBOR map but not a known alg. Per
	// RFC 9052 §3 we should preserve the parameter rather than reject
	// the whole header — high-level sign/verify routines will fail
	// later with a clear error if the algorithm is actually needed.
	bad := hex.decode('A10139' + '03E7')! // -1000 = negative arg 999 (0x03E7)
	parsed := parse_protected(bad)!
	assert parsed.algorithm == none
	assert parsed.extra_int_labels.len == 1
	assert parsed.extra_int_labels[0].label == 1
}

fn test_text_algorithm_falls_back_to_extra_label() {
	parsed := parse_protected(hex.decode('a10163666f6f')!)!
	assert parsed.algorithm == none
	assert parsed.extra_int_labels.len == 1
	assert parsed.extra_int_labels[0].label == 1
	assert parsed.extra_int_labels[0].value.as_string() == ?string('foo')
}

fn test_rejects_numeric_content_type_above_coap_range() {
	if _ := parse_protected(hex.decode('a1031a00010000')!) {
		assert false, 'numeric content types above 65535 must be rejected'
	} else {
		assert err.msg().contains('content type exceeds 65535')
	}
	if _ := check_protected_headers(Headers{
		content_type_int: u64(65536)
	}, Headers{}) {
		assert false, 'message creation must reject out-of-range content types'
	} else {
		assert err.msg().contains('content type exceeds 65535')
	}
}

fn test_rejects_iv_and_partial_iv_in_same_security_layer() {
	for buckets in [
		[
			Headers{
				iv:         [u8(1)]
				partial_iv: [u8(2)]
			},
			Headers{},
		],
		[
			Headers{
				iv: [u8(1)]
			},
			Headers{
				partial_iv: [u8(2)]
			},
		],
	] {
		if _ := check_protected_headers(buckets[0], buckets[1]) {
			assert false, 'iv and partial iv must be mutually exclusive'
		} else {
			assert err.msg().contains('iv and partial iv')
		}
	}
}

fn test_encode_rejects_duplicate_header_labels() {
	for headers in [
		Headers{
			algorithm:        .es256
			extra_int_labels: [HeaderEntry{
				label: 1
				value: cbor.new_int(-7)
			}]
		},
		Headers{
			extra_int_labels: [
				HeaderEntry{
					label: 42
					value: cbor.new_int(1)
				},
				HeaderEntry{
					label: 42
					value: cbor.new_int(2)
				},
			]
		},
		Headers{
			extra_text_labels: [
				TextHeaderEntry{
					label: 'private'
					value: cbor.new_int(1)
				},
				TextHeaderEntry{
					label: 'private'
					value: cbor.new_int(2)
				},
			]
		},
	] {
		if _ := headers.encode_map() {
			assert false, 'duplicate labels must be rejected before encoding'
		} else {
			assert err.msg().contains('duplicate header label')
		}
	}
}

fn test_message_encoders_validate_unprotected_headers() {
	invalid := Headers{
		kid:              'kid'.bytes()
		extra_int_labels: [HeaderEntry{
			label: label_kid
			value: cbor.new_bytes('duplicate'.bytes())
		}]
	}
	if _ := Sign1Message{
		unprotected: invalid
	}.encode(false) {
		assert false, 'Sign1 encoder must validate unprotected headers'
	}
	if _ := Mac0Message{
		unprotected: invalid
	}.encode(false) {
		assert false, 'Mac0 encoder must validate unprotected headers'
	}
	if _ := SignMessage{
		unprotected: invalid
		signatures:  [Signature{}]
	}.encode(false) {
		assert false, 'Sign encoder must validate unprotected headers'
	}
	if _ := MacMessage{
		unprotected: invalid
		recipients:  [Recipient{}]
	}.encode(false) {
		assert false, 'Mac encoder must validate unprotected headers'
	}
}

fn test_parse_rejects_duplicate_int_labels() {
	// map with duplicate alg labels: {1: -7, 1: -8}
	dup := hex.decode('A201260127')!
	if _ := parse_protected(dup) {
		assert false, 'duplicate integer labels must be rejected'
	} else {
		assert err is MalformedMessage
		assert err.msg().contains('duplicate header label 1')
	}
}

fn test_parse_rejects_duplicate_text_labels() {
	// map with duplicate text labels: {"x": 0, "x": 1}
	dup := hex.decode('A2617800617801')!
	if _ := parse_protected(dup) {
		assert false, 'duplicate text labels must be rejected'
	} else {
		assert err is MalformedMessage
		assert err.msg().contains('duplicate header label "x"')
	}
}

fn test_rejects_duplicate_critical_labels() {
	for encoded in [
		hex.decode('a2012602820101')!,
		hex.decode('a2028261786178617800')!,
	] {
		if _ := parse_protected(encoded) {
			assert false, 'crit labels must be unique'
		} else {
			assert err.msg().contains('crit contains duplicate label')
		}
	}
}

fn test_rejects_labels_repeated_across_header_buckets() {
	mut protected := Headers{}
	protected.algorithm = .es256
	mut unprotected := Headers{}
	unprotected.algorithm = .es256
	if _ := check_protected_headers(protected, unprotected) {
		assert false, 'integer labels must not appear in both header buckets'
	} else {
		assert err.msg().contains('both protected and unprotected')
	}

	protected = Headers{
		extra_text_labels: [
			TextHeaderEntry{
				label: 'app'
				value: cbor.new_int(1)
			},
		]
	}
	unprotected = Headers{
		extra_text_labels: [
			TextHeaderEntry{
				label: 'app'
				value: cbor.new_int(2)
			},
		]
	}
	if _ := check_protected_headers(protected, unprotected) {
		assert false, 'text labels must not appear in both header buckets'
	} else {
		assert err.msg().contains('both protected and unprotected')
	}
}

fn test_rejects_present_empty_crit() {
	if _ := parse_protected(hex.decode('a10280')!) {
		assert false, 'a present crit array must not be empty'
	} else {
		assert err.msg().contains('crit array must not be empty')
	}
}

fn test_preserves_text_labels_in_crit_for_validation() {
	mut h := Headers{}
	h.algorithm = .es256
	h.critical = [i64(1)]
	h.critical_text = ['app']
	h.extra_text_labels = [
		TextHeaderEntry{
			label: 'app'
			value: cbor.new_int(1)
		},
	]
	parsed := parse_protected(h.encode_protected()!)!
	assert parsed.critical == [i64(1)]
	assert parsed.critical_text == ['app']
	if _ := check_protected_headers(parsed, Headers{}) {
		assert false, 'text crit labels must be assessed during validation'
	} else {
		assert err.msg().contains('crit lists unknown label "app"')
	}
}

fn test_crit_rejects_its_own_label() {
	protected := Headers{
		critical: [i64(2)]
	}
	if _ := check_protected_headers(protected, Headers{}) {
		assert false, 'crit must not list label 2 itself'
	} else {
		assert err.msg().contains('crit must not list itself')
	}
}
