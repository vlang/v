import encoding.base64

struct testpair {
	decoded string
	encoded string
}

const (
	pairs = [
		// RFC 3548 examples
		testpair{'\x14\xfb\x9c\x03\xd9\x7e', 'FPucA9l+'},
		testpair{'\x14\xfb\x9c\x03\xd9', 'FPucA9k='},
		testpair{'\x14\xfb\x9c\x03', 'FPucAw=='},

		// RFC 4648 examples
		testpair{'', ''},
		testpair{'f', 'Zg=='},
		testpair{'fo', 'Zm8='},
		testpair{'foo', 'Zm9v'},
		testpair{'foob', 'Zm9vYg=='},
		testpair{'fooba', 'Zm9vYmE='},
		testpair{'foobar', 'Zm9vYmFy'},

		// Wikipedia examples
		testpair{'sure.', 'c3VyZS4='},
		testpair{'sure', 'c3VyZQ=='},
		testpair{'sur', 'c3Vy'},
		testpair{'su', 'c3U='},
		testpair{'leasure.', 'bGVhc3VyZS4='},
		testpair{'easure.', 'ZWFzdXJlLg=='},
		testpair{'asure.', 'YXN1cmUu'},
		testpair{'sure.', 'c3VyZS4='},
	]
)

fn test_decode() {
	assert base64.decode('TWFuIGlzIGRpc3Rpbmd1aXNoZWQsIG5vdCBvbmx5IGJ5IGhpcyByZWFzb24sIGJ1dCBieSB0aGlzIHNpbmd1bGFyIHBhc3Npb24gZnJvbSBvdGhlciBhbmltYWxzLCB3aGljaCBpcyBhIGx1c3Qgb2YgdGhlIG1pbmQsIHRoYXQgYnkgYSBwZXJzZXZlcmFuY2Ugb2YgZGVsaWdodCBpbiB0aGUgY29udGludWVkIGFuZCBpbmRlZmF0aWdhYmxlIGdlbmVyYXRpb24gb2Yga25vd2xlZGdlLCBleGNlZWRzIHRoZSBzaG9ydCB2ZWhlbWVuY2Ugb2YgYW55IGNhcm5hbCBwbGVhc3VyZS4=')
	== 'Man is distinguished, not only by his reason, but by this singular passion from other animals, which is a lust of the mind, that by a perseverance of delight in the continued and indefatigable generation of knowledge, exceeds the short vehemence of any carnal pleasure.'

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
	assert base64.encode('Man is distinguished, not only by his reason, but by this singular passion from other animals, which is a lust of the mind, that by a perseverance of delight in the continued and indefatigable generation of knowledge, exceeds the short vehemence of any carnal pleasure.')
	== 'TWFuIGlzIGRpc3Rpbmd1aXNoZWQsIG5vdCBvbmx5IGJ5IGhpcyByZWFzb24sIGJ1dCBieSB0aGlzIHNpbmd1bGFyIHBhc3Npb24gZnJvbSBvdGhlciBhbmltYWxzLCB3aGljaCBpcyBhIGx1c3Qgb2YgdGhlIG1pbmQsIHRoYXQgYnkgYSBwZXJzZXZlcmFuY2Ugb2YgZGVsaWdodCBpbiB0aGUgY29udGludWVkIGFuZCBpbmRlZmF0aWdhYmxlIGdlbmVyYXRpb24gb2Yga25vd2xlZGdlLCBleGNlZWRzIHRoZSBzaG9ydCB2ZWhlbWVuY2Ugb2YgYW55IGNhcm5hbCBwbGVhc3VyZS4='

	for i, p in pairs {
		got := base64.encode(p.decoded)
		if got != p.encoded {
			eprintln('pairs[${i}]: expected = ${p.encoded}, got = ${got}')
			assert false
		}
	}
}
