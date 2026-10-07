module json2

struct CachedKeyMessage {
	missing string @[omitempty]
	id      int
	text    string @[json: 'body']
	hidden  string @[skip]
	opt     ?int
}

fn test_cached_keys_preserve_omission_and_append_buffer_ownership() {
	mut output := []u8{cap: 1}
	output << `!`
	encode_append(CachedKeyMessage{ id: 7, text: 'hello' }, mut output)
	assert output.bytestr() == '!{"id":7,"body":"hello"}'
	retained := output.clone()
	output.clear()
	encode_append(CachedKeyMessage{ missing: 'first', id: 8, text: '\n', opt: 9 }, mut output)
	assert output.bytestr() == '{"missing":"first","id":8,"body":"\\n","opt":9}'
	assert retained.bytestr() == '!{"id":7,"body":"hello"}'
}

fn test_cached_keys_match_uncached_encoding_for_every_byte_and_layout() {
	for offset in [0, 1, 7, 8, 15, 16, 31] {
		for b in 0 .. 256 {
			key := 'a'.repeat(offset) + [u8(b)].bytestr() + 'tail'
			info := encoder_field_info(key, [])
			for escape_unicode in [false, true] {
				for layout in 0 .. 3 {
					options := EncoderOptions{
						escape_unicode: escape_unicode
						prettify:       layout > 0
						legacy_layout:  layout == 2
						indent_string:  '--'
						newline_string: '\r\n'
					}
					for first in [false, true] {
						mut cached := Encoder{ EncoderOptions: options, output: []u8{cap: 32} }
						mut reference := Encoder{ EncoderOptions: options, output: []u8{cap: 32} }
						assert cached.encode_cached_struct_key(first, info) == reference.encode_object_key(first,
							key)
						assert cached.output == reference.output, 'offset ${offset}, byte ${b}'
						assert cached.level == reference.level
						assert cached.prefix == reference.prefix
					}
				}
			}
		}
	}
}

struct ColdCachedKeyMessage {
	id   int
	text string @[json: 'body']
}

fn cached_key_worker(ready chan bool, start chan bool, done chan bool) {
	ready <- true
	_ := <-start
	for _ in 0 .. 50 {
		assert encode(ColdCachedKeyMessage{ id: 1, text: 'x' }) == '{"id":1,"body":"x"}'
		assert encode(ColdCachedKeyMessage{ id: 1, text: 'x' }, prettify: true) == '{\n    "id": 1,\n    "body": "x"\n}'
	}
	done <- true
}

fn test_cached_keys_publish_immutable_metadata_on_concurrent_first_use() {
	ready := chan bool{cap: 8}
	start := chan bool{cap: 8}
	done := chan bool{cap: 8}
	for _ in 0 .. 8 {
		spawn cached_key_worker(ready, start, done)
	}
	for _ in 0 .. 8 {
		_ := <-ready
	}
	for _ in 0 .. 8 {
		start <- true
	}
	for _ in 0 .. 8 {
		_ := <-done
	}
}
