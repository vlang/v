module base32

struct WrappedCase {
	payload string
	want    string
}

// Base32 output is normally wrapped at a line boundary. The `decode`
// documentation says "\r and \n are ignored", but the strip_newlines call
// was commented out and the `0xFF` sentinel is unreachable, so a wrapped
// payload was rejected as corrupt input instead of decoding.
const wrapped_cases = [
	WrappedCase{'MZXW6===', 'foo'},
	WrappedCase{'MZXW6===\r\n', 'foo'},
	WrappedCase{'MZXW6===\n', 'foo'},
	WrappedCase{'\r\nMZXW6===', 'foo'},
	WrappedCase{'MZXW6\r\nYTB', 'fooba'},
	WrappedCase{'MZ\rXW6\r===\n', 'foo'},
]

fn test_decode_ignores_newlines_as_documented() {
	e := new_std_encoding()
	for c in wrapped_cases {
		got := e.decode_string_to_string(c.payload) or {
			assert false, 'decode should have accepted "${c.payload}": ${err}'
			''
		}
		assert got == c.want, 'decoded "${got}" from "${c.payload}", want "${c.want}"'
	}
}

// NOTE: a byte outside the alphabet is *not* rejected — the decoder only
// reports the `0xFF` sentinel, which nothing in the decode map produces, so
// `?` is silently read as digit 0. That is a separate defect and is pinned in
// base32_extended_test.v rather than asserted here.
fn test_decode_leaves_a_clean_payload_unchanged() {
	e := new_std_encoding()
	assert e.decode_string_to_string('MZXW6===')! == 'foo'
}

fn test_the_package_level_decoder_agrees() {
	assert base32.decode('MZXW\n6==='.bytes())! == 'foo'.bytes()
	assert base32.decode('MZXW6==='.bytes())! == 'foo'.bytes()
}
