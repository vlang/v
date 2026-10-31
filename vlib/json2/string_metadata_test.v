module json2

struct MetadataMessage {
	text  string
	nonce string
}

fn check_metadata_tokens(input string, buffer &DecodeBuffer) {
	for info in buffer.values_info {
		if info.value_kind == .string {
			body := input[info.position + 1..info.position + info.length - 1]
			assert info.has_escape == body.contains('\\'), 'position ${info.position}'
		} else {
			assert !info.has_escape, 'position ${info.position}'
		}
	}
}

fn test_string_metadata_roundtrips_boundaries_unicode_and_escape_positions() ! {
	mut buffer := DecodeBuffer{}
	for size in [0, 1, 7, 8, 9, 15, 16, 17, 63, 64, 65, 255, 1023, 1024, 4096] {
		plain := 'x'.repeat(size)
		for special in ['', '"', '\\', '\x00\b\f\n\r\t', 'é😀', '/'] {
			for text in [special + plain, plain + special, plain + special + plain] {
				for escape_unicode in [false, true] {
					wire := ' \t' + encode(text, escape_unicode: escape_unicode)
					assert decode[string](wire)! == text, 'size ${size}'
					assert decode_reuse[string](wire, mut buffer)! == text, 'size ${size}'
					check_metadata_tokens(wire, buffer)
				}
			}
		}
	}
}

fn test_string_metadata_tracks_keys_nested_values_and_reused_slots() ! {
	mut buffer := DecodeBuffer{}
	for _ in 0 .. 20 {
		for wire in [r'{"te\u0078t":"hello","nonce":"\u0061"}', '{"text":"hello","nonce":"a"}',
			r'{"ignored":["\n",{"\u006b":"\t"},false,1,null],"text":"hello","nonce":"a"}',
			'{"text":"hello","nonce":"a","ignored":[{},[],"",0]}'] {
			value := decode_reuse[MetadataMessage](wire, mut buffer)!
			assert value == MetadataMessage{ text: 'hello', nonce: 'a' }
			check_metadata_tokens(wire, buffer)
		}
		assert decode_reuse[int]('42', mut buffer)! == 42
		check_metadata_tokens('42', buffer)
		assert decode_reuse[string](r'"\n"', mut buffer)! == '\n'
		assert decode_reuse[string]('"plain"', mut buffer)! == 'plain'
		check_metadata_tokens('"plain"', buffer)
	}
}

fn test_string_metadata_recovers_after_malformed_escapes_and_preserves_values() ! {
	mut buffer := DecodeBuffer{}
	retained := decode_reuse[MetadataMessage](r'{"text":"\ud83d\ude00","nonce":"old"}', mut buffer)!
	for input in [r'"\x"', r'"\u12"', r'"\uQQQQ"', r'"\ud800"', r'"\udc00"', r'"\ud800\u0041"',
		r'{"text":"unterminated', r'["\n"', r'{"text":"a\"'] {
		mut rejected := false
		decode_reuse[Any](input, mut buffer) or { rejected = true }
		assert rejected
		assert decode_reuse[string]('"plain"', mut buffer)! == 'plain'
		check_metadata_tokens('"plain"', buffer)
		assert decode_reuse[string](r'"\t\u00e9"', mut buffer)! == '\té'
		check_metadata_tokens(r'"\t\u00e9"', buffer)
	}
	assert retained == MetadataMessage{ text: '😀', nonce: 'old' }
}
