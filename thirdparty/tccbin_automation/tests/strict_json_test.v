module tests

import tccbin_automation.bin

fn test_strict_json_rejects_ambiguous_lexical_forms() {
	invalid := [
		'{"a":1,"a":2}',
		'{"n":-0}',
		'{"n":01}',
		'{"n":1.5}',
		'{"n":1e3}',
		'{"n":9007199254740992}',
		'{"n":-9007199254740992}',
		'{"s":"\\uD800"}',
		'{"s":"\\uDC00"}',
		'{"s":"\\x20"}',
	]
	for source in invalid {
		bin.parse_strict_json(source) or { continue }
		assert false, 'accepted forbidden JSON: ${source}'
	}
}

fn test_strict_json_rejects_bom_and_invalid_utf8_before_decoding() {
	bom := [u8(0xef), u8(0xbb), u8(0xbf)].bytestr() + '{}'
	invalid_utf8 := [u8(0xff)].bytestr()
	for source in [bom, invalid_utf8] {
		bin.parse_strict_json(source) or { continue }
		assert false, 'accepted forbidden byte-level JSON input'
	}
}

fn test_jcs_is_stable_and_uses_utf16_key_order() {
	value := bin.parse_strict_json('{"b":1,"a":2,"😀":3,"\ue000":4}') or { panic(err) }
	assert bin.canonical_json(value) == '{"a":2,"b":1,"😀":3,"":4}'
	assert bin.json_sha256(value) == bin.json_sha256(bin.parse_strict_json(bin.canonical_json(value)) or {
		panic(err)
	})
}

fn test_jcs_preserves_valid_surrogate_pairs_as_utf8() {
	value := bin.parse_strict_json('{"rocket":"\\uD83D\\uDE80"}') or { panic(err) }
	assert bin.canonical_json(value) == '{"rocket":"🚀"}'
}

fn test_strict_json_preserves_surrogate_pair_boundaries() {
	for source, expected in {
		'"\\uD800\\uDC00"': 0x10000
		'"\\uDBFF\\uDFFF"': 0x10ffff
	} {
		value := bin.parse_strict_json(source) or { panic(err) }
		runes := value.string_value.runes()
		assert runes.len == 1
		assert runes[0] == rune(expected)
	}
}
