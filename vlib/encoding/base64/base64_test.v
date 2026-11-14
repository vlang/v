import encoding.base64

struct TestPair {
	decoded string
	encoded string
}

const pairs = [
	// RFC 3548 examples
	TestPair{'\x14\xfb\x9c\x03\xd9\x7e', 'FPucA9l+'},
	TestPair{'\x14\xfb\x9c\x03\xd9', 'FPucA9k='},
	TestPair{'\x14\xfb\x9c\x03', 'FPucAw=='},
	// RFC 4648 examples
	TestPair{'', ''},
	TestPair{'f', 'Zg=='},
	TestPair{'fo', 'Zm8='},
	TestPair{'foo', 'Zm9v'},
	TestPair{'foob', 'Zm9vYg=='},
	TestPair{'fooba', 'Zm9vYmE='},
	TestPair{'foobar', 'Zm9vYmFy'},
	// Wikipedia examples
	TestPair{'sure.', 'c3VyZS4='},
	TestPair{'sure', 'c3VyZQ=='},
	TestPair{'sur', 'c3Vy'},
	TestPair{'su', 'c3U='},
	TestPair{'leasure.', 'bGVhc3VyZS4='},
	TestPair{'easure.', 'ZWFzdXJlLg=='},
	TestPair{'asure.', 'YXN1cmUu'},
	TestPair{'sure.', 'c3VyZS4='},
]

const man_pair = TestPair{'Man is distinguished, not only by his reason, but by this singular passion from other animals, which is a lust of the mind, that by a perseverance of delight in the continued and indefatigable generation of knowledge, exceeds the short vehemence of any carnal pleasure.', 'TWFuIGlzIGRpc3Rpbmd1aXNoZWQsIG5vdCBvbmx5IGJ5IGhpcyByZWFzb24sIGJ1dCBieSB0aGlzIHNpbmd1bGFyIHBhc3Npb24gZnJvbSBvdGhlciBhbmltYWxzLCB3aGljaCBpcyBhIGx1c3Qgb2YgdGhlIG1pbmQsIHRoYXQgYnkgYSBwZXJzZXZlcmFuY2Ugb2YgZGVsaWdodCBpbiB0aGUgY29udGludWVkIGFuZCBpbmRlZmF0aWdhYmxlIGdlbmVyYXRpb24gb2Yga25vd2xlZGdlLCBleGNlZWRzIHRoZSBzaG9ydCB2ZWhlbWVuY2Ugb2YgYW55IGNhcm5hbCBwbGVhc3VyZS4='}

fn test_decode() {
	assert base64.decode(man_pair.encoded) == man_pair.decoded.bytes()

	// Test for incorrect padding.
	assert base64.decode('aGk') == ''.bytes()
	assert base64.decode('aGk=') == 'hi'.bytes()
	assert base64.decode('aGk==') == ''.bytes()

	for i, p in pairs {
		got := base64.decode(p.encoded)
		if got != p.decoded.bytes() {
			eprintln('pairs[${i}]: expected = ${p.decoded}, got = ${got}')
			assert false
		}
	}
}

fn test_decode_str() {
	assert base64.decode_str(man_pair.encoded) == man_pair.decoded

	// Test for incorrect padding.
	assert base64.decode_str('aGk') == ''
	assert base64.decode_str('aGk=') == 'hi'
	assert base64.decode_str('aGk==') == ''

	for i, p in pairs {
		got := base64.decode_str(p.encoded)
		if got != p.decoded {
			eprintln('pairs[${i}]: expected = ${p.decoded}, got = ${got}')
			assert false
		}
	}
}

fn test_encode() {
	assert base64.encode(man_pair.decoded.bytes()) == man_pair.encoded

	for i, p in pairs {
		got := base64.encode(p.decoded.bytes())
		if got != p.encoded {
			eprintln('pairs[${i}]: expected = ${p.encoded}, got = ${got}')
			assert false
		}
	}
}

fn test_encode_str() {
	assert base64.encode_str(man_pair.decoded) == man_pair.encoded

	for i, p in pairs {
		got := base64.encode_str(p.decoded)
		if got != p.encoded {
			eprintln('pairs[${i}]: expected = ${p.encoded}, got = ${got}')
			assert false
		}
	}
}

fn test_url_encode() {
	test := base64.url_encode('Hello Base64Url encoding!'.bytes())
	assert test == 'SGVsbG8gQmFzZTY0VXJsIGVuY29kaW5nIQ'
}

fn test_url_encode_str() {
	test := base64.url_encode_str('Hello Base64Url encoding!')
	assert test == 'SGVsbG8gQmFzZTY0VXJsIGVuY29kaW5nIQ'
}

fn test_url_decode() {
	test := base64.url_decode('SGVsbG8gQmFzZTY0VXJsIGVuY29kaW5nIQ')
	assert test == 'Hello Base64Url encoding!'.bytes()
}

fn test_url_decode_str() {
	test := base64.url_decode_str('SGVsbG8gQmFzZTY0VXJsIGVuY29kaW5nIQ')
	assert test == 'Hello Base64Url encoding!'
}

fn test_encode_null_u8() {
	assert base64.encode([u8(`A`), 0, `C`]) == 'QQBD'
}

fn test_encode_null_byte_str() {
	// While this works, bytestr() does a memcpy
	s := [u8(`A`), 0, `C`].bytestr()
	assert base64.encode_str(s) == 'QQBD'
}

fn test_decode_null_u8() {
	assert base64.decode('QQBD') == [u8(`A`), 0, `C`]
}

fn test_decode_null_byte_str() {
	// While this works, bytestr() does a memcpy
	s := [u8(`A`), 0, `C`].bytestr()
	assert base64.decode_str('QQBD') == s
}

fn test_decode_in_buffer_bytes() {
	rfc4648_pairs := [
		TestPair{'foob', 'Zm9vYg=='},
		TestPair{'fooba', 'Zm9vYmE='},
		TestPair{'foobar', 'Zm9vYmFy'},
	]
	mut src_dec_buf := []u8{len: 8}
	mut src_enc_buf := []u8{len: 8}
	mut out_buf := []u8{len: 8}

	for p in rfc4648_pairs {
		src_dec_buf = p.decoded.bytes()
		src_enc_buf = p.encoded.bytes()
		n := base64.decode_in_buffer_bytes(src_enc_buf, out_buf.data)
		assert src_dec_buf == out_buf[..n]
	}
}

fn test_decode_strict_accepts_canonical_base64() {
	for p in pairs {
		assert base64.decode_strict(p.encoded)! == p.decoded.bytes()
	}
	for n in 0 .. 64 {
		data := []u8{len: n, init: u8(index * 37 + 11)}
		assert base64.decode_strict(base64.encode(data))! == data
	}
}

fn test_decode_strict_reports_the_offset_of_invalid_input() {
	cases := {
		'!!!!':         0
		'QUJ':          3
		'QUJDR':        5
		'Zm9v\n':       4
		'Zm9 ':         3
		'Zm9v\r\nZm9v': 4
		'ab-_':         2
		'QQ=A':         3
		'=QQQ':         0
		'====':         0
		'Q===':         1
		'Zm9vYmFy===':  8
		'QQQ==':        4
		'QQ=':          3
		'Zh==':         1
		'Zm9=':         2
		'Zm\xc3\xa9':   2
		'Zh==A':        1
		'Zh=A':         1
	}
	for input, offset in cases {
		if decoded := base64.decode_strict(input) {
			assert false, '`${input}` decoded as ${decoded}'
		} else {
			if err is base64.CorruptInputError {
				assert err.offset == offset, '`${input}`'
				assert err.msg() == 'base64: invalid input at byte ${offset}'
			} else {
				assert false, '`${input}`: ${err}'
			}
		}
	}
}

fn test_decode_strict_rejects_what_decode_accepts_silently() {
	assert base64.decode('!!!!') == [u8(0), 0, 0]
	assert base64.decode('Zh==') == 'f'.bytes()
	assert base64.decode_strict('!!!!') or { []u8{} } == []u8{}
	assert base64.decode_strict('Zh==') or { []u8{} } == []u8{}
}

fn test_decode_strict_accepts_a_padded_group_only_if_it_is_canonical() {
	alphabet := 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'
	for c in alphabet {
		for group in ['Q${c.ascii_str()}==', 'QQ${c.ascii_str()}='] {
			canonical := base64.encode(base64.decode(group)) == group
			if _ := base64.decode_strict(group) {
				assert canonical, group
			} else {
				assert !canonical, group
			}
		}
	}
}
