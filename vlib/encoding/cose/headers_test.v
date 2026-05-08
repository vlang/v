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
