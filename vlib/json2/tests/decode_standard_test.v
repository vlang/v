import json2

struct StandardItem {
	name string @[json: 'displayName']
	ids  []int
	meta map[string]string
	note ?string
	port int = 8080
}

fn test_decode_standard_preserves_json_decoding() {
	text := '{"displayName":"V","ids":[1,2],"meta":{"key":"value"},"note":"hello"}'
	got := json2.decode_standard[StandardItem](text)!
	assert got == json2.decode[StandardItem](text)!
	assert got.name == 'V'
	assert got.ids == [1, 2]
	assert got.meta == {
		'key': 'value'
	}
	assert got.note? == 'hello'
	assert got.port == 8080
	assert json2.decode_standard[[]int]('[1,2]')! == [1, 2]
	assert json2.decode_standard[map[string]int]('{"one":1}')! == {
		'one': 1
	}
}

fn test_decode_standard_preserves_lenient_defaults() {
	assert json2.decode_standard[int]('"123"')! == 123
	assert json2.decode_standard[int]('null')! == 0
	assert json2.decode_standard[[2]int]('[1]')! == [1, 0]!
}

fn test_decode_standard_preserves_strict_options() {
	assert json2.decode_standard[int]('123', strict: true)! == 123
	assert json2.decode_standard[[2]int]('[1,2]', strict: true)! == [1, 2]!
	for text in ['"123"', 'null'] {
		json2.decode_standard[int](text, strict: true) or { continue }
		assert false, 'strict decoding should reject ${text}'
	}
	for text in ['[1]', '[1,2,3]'] {
		json2.decode_standard[[2]int](text, strict: true) or { continue }
		assert false, 'strict decoding should reject ${text}'
	}
}

fn test_decode_standard_always_rejects_json5_syntax() {
	for text in ['{name:"V"}', "{'name':'V'}", '{"name":"V",}', '/*comment*/{}', '{"ids":[0x10]}',
		'{"ids":[+1]}', '{"ids":[.5]}'] {
		json2.decode_standard[StandardItem](text) or { continue }
		assert false, 'standard decoding should reject ${text}'
	}
	assert json2.decode_standard[int]('123', json5: true)! == 123
	json2.decode_standard[StandardItem]('{name:"V"}', json5: true) or { return }
	assert false, 'json5 must not enable JSON5 syntax in decode_standard'
}

fn test_decode_standard_preserves_errors() {
	for text in ['', '{', '{"ids":[1}', '{"ids":["invalid"]}'] {
		json2.decode_standard[StandardItem](text) or {
			message := err.msg()
			code := err.code()
			json2.decode[StandardItem](text) or {
				assert err.msg() == message
				assert err.code() == code
				continue
			}
			assert false, 'decode should reject ${text}'
			continue
		}
		assert false, 'decode_standard should reject ${text}'
	}
}
