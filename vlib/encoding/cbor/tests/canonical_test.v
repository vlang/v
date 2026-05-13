// Canonical encoding (RFC 8949 §4.2.1, "core deterministic encoding"):
// map keys are sorted by length-first lexicographic order of their
// encoded forms. Used for hashable / signable payloads.
module main

import encoding.cbor
import encoding.hex

fn h(s string) []u8 {
	return hex.decode(s) or { panic('invalid hex: ${s}') }
}

fn beq(a []u8, b []u8) bool {
	if a.len != b.len {
		return false
	}
	for i in 0 .. a.len {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}

fn test_canonical_sorts_text_keys() {
	// Build a map with reverse-alphabetic insertion order; canonical
	// output should still emit keys "a", "b", "c", "d", "e".
	v := cbor.Value(cbor.Map{
		pairs: [
			cbor.MapPair{
				key:   cbor.Value(cbor.Text{
					value: 'e'
				})
				value: cbor.Value(cbor.Text{
					value: 'E'
				})
			},
			cbor.MapPair{
				key:   cbor.Value(cbor.Text{
					value: 'b'
				})
				value: cbor.Value(cbor.Text{
					value: 'B'
				})
			},
			cbor.MapPair{
				key:   cbor.Value(cbor.Text{
					value: 'd'
				})
				value: cbor.Value(cbor.Text{
					value: 'D'
				})
			},
			cbor.MapPair{
				key:   cbor.Value(cbor.Text{
					value: 'a'
				})
				value: cbor.Value(cbor.Text{
					value: 'A'
				})
			},
			cbor.MapPair{
				key:   cbor.Value(cbor.Text{
					value: 'c'
				})
				value: cbor.Value(cbor.Text{
					value: 'C'
				})
			},
		]
	})
	got := cbor.encode_value(v, cbor.EncodeOpts{ canonical: true })!
	want := h('a56161614161626142616361436164614461656145')
	assert beq(got, want), 'canonical: got ${hex.encode(got)}, want ${hex.encode(want)}'
}

fn test_canonical_length_first_then_lex() {
	// Length-first ordering: shorter keys first.
	// {"a": 1, "aa": 2}  →  short before long.
	v := cbor.Value(cbor.Map{
		pairs: [
			cbor.MapPair{
				key:   cbor.Value(cbor.Text{
					value: 'aa'
				})
				value: cbor.Value(cbor.new_uint(2))
			},
			cbor.MapPair{
				key:   cbor.Value(cbor.Text{
					value: 'a'
				})
				value: cbor.Value(cbor.new_uint(1))
			},
		]
	})
	got := cbor.encode_value(v, cbor.EncodeOpts{ canonical: true })!
	// Encoded keys "a"=0x6161 (2 bytes), "aa"=0x626161 (3 bytes).
	// Length-first: "a" first, then "aa".
	// Result: a2 61 61 01 62 61 61 02
	want := h('a2616101626161 02'.replace(' ', ''))
	assert beq(got, want), 'length-first: got ${hex.encode(got)}'
}

fn test_self_describe_prefix() {
	bytes := cbor.encode[u64](u64(0), cbor.EncodeOpts{ self_describe: true })!
	// Magic prefix: d9 d9 f7 then 0x00.
	assert beq(bytes, [u8(0xd9), 0xd9, 0xf7, 0x00])
}

// ---------------------------------------------------------------------
// Struct-as-map canonical encoding: declaration order MUST NOT leak
// into the wire form when canonical mode is on. Otherwise hash- or
// signature-based payloads (COSE, CWT, DAG-CBOR) lose stability across
// V versions whenever a field is added or reordered in source.
// ---------------------------------------------------------------------

struct OutOfOrder {
	zeta  int
	alpha int
	mid   int
}

fn test_canonical_struct_sorts_keys_by_encoded_form() {
	v := OutOfOrder{
		zeta:  1
		alpha: 2
		mid:   3
	}
	got := cbor.encode[OutOfOrder](v, cbor.EncodeOpts{ canonical: true })!
	// Length-first lex on encoded keys: "mid" (4B) < "zeta" (5B) < "alpha" (6B).
	// a3 636d6964 03 647a657461 01 65616c706861 02
	want := h('a3636d69640364 7a65746101 65616c706861 02'.replace(' ', ''))
	assert beq(got, want), 'declaration order leaked: got ${hex.encode(got)}'
}

fn test_canonical_struct_preserves_declaration_order_when_off() {
	// Default (non-canonical) keeps source order — important for human
	// inspection and matches the documented permissive behaviour.
	v := OutOfOrder{
		zeta:  1
		alpha: 2
		mid:   3
	}
	got := cbor.encode[OutOfOrder](v, cbor.EncodeOpts{})!
	// a3 647a657461 01 65616c706861 02 636d6964 03
	want := h('a3647a65746101 65616c706861 02 636d6964 03'.replace(' ', ''))
	assert beq(got, want), 'non-canonical reorder: got ${hex.encode(got)}'
}
