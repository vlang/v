module json2

struct ReuseIncoming {
	kind  string @[json: 'type']
	to    int
	text  string
	nonce string
}

struct ReuseOptional {
	name  string = 'default'
	count ?int
	items []string
}

fn reuse_matches[T](input string, mut buffer DecodeBuffer, options DecoderOptions) {
	mut expected_error := ''
	expected := decode[T](input, options) or {
		expected_error = err.msg()
		T{}
	}
	mut actual_error := ''
	actual := decode_reuse[T](input, mut buffer, options) or {
		actual_error = err.msg()
		T{}
	}
	assert actual_error == expected_error
	if expected_error == '' {
		assert encode(actual) == encode(expected)
	}
}

fn test_decode_reuse_matches_values_options_and_errors() {
	mut buffer := DecodeBuffer{}
	for strict in [false, true] {
		options := DecoderOptions{ strict: strict }
		for input in [
			'{"type":"send","to":2,"text":"hello","nonce":"a"}',
			'{"text":"مرحبا é 😀","to":"2","type":"send"}',
			'{"te\\u0078t":"\\u00e9\\ud83d\\ude00","to":2}',
			'{"text":null,"to":null}',
			'{}',
			'null',
			'[]',
			'true',
			'"string"',
			'{"unknown":[{"nested":[null,true,1.25e-3]}],"text":"ok"}',
			'{"type":"ping","type":"send","to":2,"text":"duplicate"}',
			'',
			' ',
			'{',
			'{"text":',
			'{"text":"bad\\x"}',
			'{"text":"\\u12"}',
			'{"to":1e1000}',
			'{"to":1.5}',
			'{"text":"unterminated}',
			'{"text":false}',
			'{"to":2} trailing',
			'{"to":2,}',
		] {
			reuse_matches[ReuseIncoming](input, mut buffer, options)
		}
		for input in ['{"count":3,"items":["a","é"]}', '{"count":null}', '{}',
			'{"name":null,"items":null}', '{"count":"3"}', '{"count":{}}'] {
			reuse_matches[ReuseOptional](input, mut buffer, options)
		}
		for input in ['[]', '[1,2,3]', '["4",null,6]', '[1,', 'null', '{}'] {
			reuse_matches[[]int](input, mut buffer, options)
		}
		for input in ['{}', '{"a":[1,2],"b":[]}', '{"a":null}', '{"a":[1,]}'] {
			reuse_matches[map[string][]int](input, mut buffer, options)
		}
		for input in ['[1,2]', '[]', '[1,2,3]', 'null', '[1,"2"]'] {
			reuse_matches[[2]int](input, mut buffer, options)
		}
	}
}

fn test_decode_reuse_grows_recovers_and_preserves_returned_values() ! {
	mut buffer := DecodeBuffer{}
	retained := decode_reuse[ReuseIncoming]('{"text":"retained 😀","nonce":"old","to":2}', mut buffer)!
	for size in [0, 1, 15, 256, 4096, 3, 0, 1024, 1] {
		text := encode([]int{len: size, init: index})
		reuse_matches[[]int](text, mut buffer, DecoderOptions{})
		reuse_matches[[]int](text[..text.len - 1], mut buffer, DecoderOptions{})
		reuse_matches[ReuseIncoming]('{"text":"after error"}', mut buffer, DecoderOptions{})
	}
	assert retained.text == 'retained 😀' && retained.nonce == 'old' && retained.to == 2
	for _ in 0 .. 100 {
		_ := decode_reuse[[]int]('[1,2,3]', mut buffer)!
	}
	storage := buffer.values_info.data
	for _ in 0 .. 100 {
		assert decode_reuse[[]int]('[4,5,6]', mut buffer)! == [4, 5, 6]
		assert buffer.values_info.data == storage
	}
}

fn test_decode_reuse_differential_truncated_and_mutated_documents() {
	mut buffer := DecodeBuffer{}
	for source in ['{"type":"send","to":2,"text":"héllo 😀","nonce":"x"}',
		'{"text":"\\\"\\\\\\n\\u1234","to":-12,"extra":[true,false,null,{}]}'] {
		for end in 0 .. source.len + 1 {
			reuse_matches[ReuseIncoming](source[..end], mut buffer, DecoderOptions{})
		}
		for index in 0 .. source.len {
			for replacement in [`{`, `}`, `[`, `]`, `:`, `,`, `"`, `\\`, `0`, `x`, ` `] {
				mut bytes := source.bytes()
				bytes[index] = replacement
				reuse_matches[ReuseIncoming](bytes.bytestr(), mut buffer, DecoderOptions{})
			}
		}
	}
}
