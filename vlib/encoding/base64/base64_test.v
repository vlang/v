import encoding.base64

struct TestPair {
	decoded string
	encoded string
}

const (
	pairs = [
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

	man_pair = TestPair{
		'Man is distinguished, not only by his reason, but by this singular passion from other animals, which is a lust of the mind, that by a perseverance of delight in the continued and indefatigable generation of knowledge, exceeds the short vehemence of any carnal pleasure.',
		'TWFuIGlzIGRpc3Rpbmd1aXNoZWQsIG5vdCBvbmx5IGJ5IGhpcyByZWFzb24sIGJ1dCBieSB0aGlzIHNpbmd1bGFyIHBhc3Npb24gZnJvbSBvdGhlciBhbmltYWxzLCB3aGljaCBpcyBhIGx1c3Qgb2YgdGhlIG1pbmQsIHRoYXQgYnkgYSBwZXJzZXZlcmFuY2Ugb2YgZGVsaWdodCBpbiB0aGUgY29udGludWVkIGFuZCBpbmRlZmF0aWdhYmxlIGdlbmVyYXRpb24gb2Yga25vd2xlZGdlLCBleGNlZWRzIHRoZSBzaG9ydCB2ZWhlbWVuY2Ugb2YgYW55IGNhcm5hbCBwbGVhc3VyZS4='
	}

)

fn test_decode() {
	assert base64.decode(man_pair.encoded) == man_pair.decoded

	// Test for incorrect padding.
	assert base64.decode('aGk') == 'hi'
	assert base64.decode('aGk=') == 'hi'
	assert base64.decode('aGk==') == 'hi'

	for i, p in pairs {
		got := base64.decode(p.encoded)
		if got != p.decoded {
			eprintln('pairs[${i}]: expected = ${p.decoded}, got = ${got}')
			assert false
		}
	}
}

fn test_encode() {
	assert base64.encode(man_pair.decoded) == man_pair.encoded

	for i, p in pairs {
		got := base64.encode(p.decoded)
		if got != p.encoded {
			eprintln('pairs[${i}]: expected = ${p.encoded}, got = ${got}')
			assert false
		}
	}
}

fn test_encode_url() {
	test := base64.encode_url('Hello Base64Url encoding!')
	assert test == 'SGVsbG8gQmFzZTY0VXJsIGVuY29kaW5nIQ'
}

fn test_decode_url() {
	test := base64.decode_url("SGVsbG8gQmFzZTY0VXJsIGVuY29kaW5nIQ")
	assert test == 'Hello Base64Url encoding!'
}
