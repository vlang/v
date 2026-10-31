module pem

// https://github.com/vlang/v/issues/30692
// A body with characters outside the base64 alphabet, or without its padding,
// was decoded to corrupt or truncated bytes instead of being rejected.

const invalid_bodies = [
	'SGVsbG8sIFBF!SE=', // a character outside the alphabet
	'SGVsbG8sIFBFTSE', // padding removed
	'SGVsbG8sIFBFTS', // padding removed, two characters short
	'SGVsbG8sIFBFT', // a length that no padding can complete
	'SGVsbG8sIFBFTSE==', // one padding character too many
	'SGVs=G8sIFBFTSE=', // padding in the middle
	'QQ==QUJD', // data after the padding
	'QUJ=D===', // a data character between padding characters
	'====', // only padding
	'QUJD\vQUJD', // whitespace that PEM does not skip
	'SGVsbG8s-_BFTSE=', // the URL-safe alphabet
]

fn test_invalid_base64_body_is_rejected() {
	for body in invalid_bodies {
		input := '-----BEGIN TEST-----\n${body}\n-----END TEST-----\n'
		if _, _ := decode(input) {
			assert false, 'decode should return `none` for the body `${body}`'
		}
		if _ := decode_only(input) {
			assert false, 'decode_only should return `none` for the body `${body}`'
		}
	}
}

fn test_invalid_base64_after_headers_is_rejected() {
	input := '-----BEGIN TEST-----\nProc-Type: 4,ENCRYPTED\n\nQUJD!\n-----END TEST-----\n'
	if _ := decode_only(input) {
		assert false, 'decode_only should return `none` for an invalid body after headers'
	}
}

fn test_valid_base64_bodies_are_decoded() {
	valid := {
		'SGVsbG8sIFBFTSE=':       'Hello, PEM!'
		'SGVsbG8sIFBFTSEh':       'Hello, PEM!!'
		'SGVsbG8sIFBFTQ==':       'Hello, PEM'
		'SGVs\nbG8s\nIFBF\nTSE=': 'Hello, PEM!'
		'SGVsbG8s\r\nIFBFTSE=':   'Hello, PEM!'
		' SGVs bG8s\tIFBF TSE= ': 'Hello, PEM!'
		'+/+/':                   [u8(0xfb), 0xff, 0xbf].bytestr()
	}
	for body, expected in valid {
		input := '-----BEGIN TEST-----\n${body}\n-----END TEST-----\n'
		block := decode_only(input) or {
			assert false, 'decode_only failed for the body `${body}`'
			return
		}
		assert block.data == expected.bytes()
	}
}

fn test_long_body_round_trip() {
	mut original := Block.new('TEST')
	for i in 0 .. 1000 {
		original.data << u8(i * 7)
	}
	for length in [1000, 64, 31, 3] {
		encoded := original.encode(line_length: length, line_ending: '\r\n') or {
			assert false, 'encode failed'
			return
		}
		assert decode_only(encoded) or { Block{} } == original
	}
}
