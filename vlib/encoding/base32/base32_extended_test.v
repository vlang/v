// Coverage for the base32 public surface that base32_test.v leaves out: the
// byte-array entry points, the Encoding methods, the alternate alphabets and
// padding modes, and the decode strictness rules.
//
// The vectors are the published ones from RFC 4648 section 10. Everything else
// retains current master validation, including rejecting bytes outside the alphabet.
import encoding.base32

struct Vector {
	decoded string
	encoded string
}

// Every sample from RFC 4648 section 10, in order, for the standard alphabet.
const rfc4648_std_vectors = [
	Vector{'', ''},
	Vector{'f', 'MY======'},
	Vector{'fo', 'MZXQ===='},
	Vector{'foo', 'MZXW6==='},
	Vector{'foob', 'MZXW6YQ='},
	Vector{'fooba', 'MZXW6YTB'},
	Vector{'foobar', 'MZXW6YTBOI======'},
]

// The same inputs against the extended hex alphabet from RFC 4648 section 6.
const rfc4648_hex_vectors = [
	Vector{'', ''},
	Vector{'f', 'CO======'},
	Vector{'fo', 'CPNG===='},
	Vector{'foo', 'CPNMU==='},
	Vector{'foob', 'CPNMUOG='},
	Vector{'fooba', 'CPNMUOJ1'},
	Vector{'foobar', 'CPNMUOJ1E8======'},
]

// A deterministic byte pattern of an arbitrary length.
fn pattern(n int) []u8 {
	mut b := []u8{len: n}
	for i in 0 .. n {
		b[i] = u8(i * 29 + 3)
	}
	return b
}

fn test_rfc4648_standard_alphabet_vectors() {
	for v in rfc4648_std_vectors {
		assert base32.encode_string_to_string(v.decoded) == v.encoded, v.decoded
		assert base32.decode_string_to_string(v.encoded)! == v.decoded, v.encoded
	}
}

fn test_rfc4648_hex_alphabet_vectors() {
	hex_encoding := base32.new_encoding(base32.hex_alphabet)
	for v in rfc4648_hex_vectors {
		assert hex_encoding.encode_string_to_string(v.decoded) == v.encoded, v.decoded
		assert hex_encoding.decode_string_to_string(v.encoded)! == v.decoded, v.encoded
	}
}

// The free byte-array entry points and their string wrappers must agree.
fn test_free_byte_api_matches_the_string_api() {
	for n in 0 .. 21 {
		b := pattern(n)
		encoded_bytes := base32.encode(b)
		assert encoded_bytes == base32.encode_string_to_string(b.bytestr()).bytes(), 'n=${n}'
		assert base32.encode_to_string(b) == base32.encode_string_to_string(b.bytestr()), 'n=${n}'
		decoded_bytes := base32.decode(encoded_bytes)!
		assert decoded_bytes == base32.decode_string_to_string(base32.encode_string_to_string(b.bytestr()))!.bytes(), 'n=${n}'
		assert base32.decode_to_string(encoded_bytes)! == b.bytestr(), 'n=${n}'
	}
}

fn test_encode_empty_input_yields_no_output() {
	assert base32.encode([]u8{}) == []u8{}
	assert base32.encode_to_string([]u8{}) == ''
	assert base32.encode_string_to_string('') == ''
	assert base32.decode([]u8{})! == []u8{}
	assert base32.decode_to_string([]u8{})! == ''
	assert base32.decode_string_to_string('')! == ''
}

// encode pads its output to a multiple of 8 characters unless padding is off.
fn test_encode_output_length_is_a_multiple_of_eight() {
	for n in 0 .. 21 {
		assert base32.encode_string_to_string(pattern(n).bytestr()).len % 8 == 0, 'n=${n}'
	}
}

fn test_encoding_methods_cover_every_wrapper() {
	b := 'foobar'.bytes()
	std := base32.new_std_encoding()
	assert std.encode_to_string(b) == 'MZXW6YTBOI======'
	assert std.encode_string_to_string('foobar') == 'MZXW6YTBOI======'
	assert std.decode_string('MZXW6YTBOI======')! == b
	assert std.decode_string_to_string('MZXW6YTBOI======')! == 'foobar'
	// decode_string and decode_string_to_string take a string, not bytes.
	assert std.decode_string('MZXW6YTBOI======')!.len == b.len
}

fn test_new_std_encoding_and_new_encoding_agree() {
	std := base32.new_std_encoding()
	explicit := base32.new_encoding(base32.std_alphabet)
	for n in 0 .. 13 {
		b := pattern(n)
		assert std.encode(b) == explicit.encode(b), 'n=${n}'
		assert std.decode(explicit.encode(b))! == b, 'n=${n}'
	}
}

fn test_new_encoding_with_padding_uses_the_given_character() {
	starred := base32.new_encoding_with_padding(base32.std_alphabet, `*`)
	assert starred.encode_string_to_string('foo') == 'MZXW6***'
	assert starred.decode_string_to_string('MZXW6***')! == 'foo'

	// `=` is the default, so an explicit `=` matches new_std_encoding.
	equals := base32.new_encoding_with_padding(base32.std_alphabet, base32.std_padding)
	assert equals.encode_string_to_string('foo') == 'MZXW6==='
}

fn test_no_padding_round_trips_at_every_length() {
	// NOTE: no_padding is u8(-1), so an `int` literal has to be avoided here.
	no_padding := base32.new_std_encoding_with_padding(base32.no_padding)
	for n in 0 .. 13 {
		b := pattern(n)
		encoded := no_padding.encode_string_to_string(b.bytestr())
		assert encoded.len == (n * 8 + 4) / 5, 'n=${n}: got ${encoded.len}'
		assert no_padding.decode_string_to_string(encoded)! == b.bytestr(), 'n=${n}'
	}
}

fn test_padding_round_trips_at_every_length() {
	for n in 0 .. 13 {
		b := pattern(n)
		encoded := base32.encode_string_to_string(b.bytestr())
		assert base32.decode_string_to_string(encoded)! == b.bytestr(), 'n=${n}'
	}
}

// RFC 4648 section 6: 1, 3 and 6 data characters in the final quantum do not
// carry enough bits to produce a whole output byte, so those lengths are
// rejected. (A leading single `=` is consumed as data because the decoder
// requires j >= 2 before it starts looking for padding, so 1 never reaches
// this check in practice -- only 3 and 6 do.)
fn test_decode_rejects_unusable_padding_lengths() {
	for s in ['AAA=====', 'AAAAAA=='] {
		if _ := base32.decode(s.bytes()) {
			assert false, '${s} must be rejected'
		}
	}
	// The usable lengths decode.
	assert base32.decode('AA======'.bytes())!.len == 1
	assert base32.decode('AAAA===='.bytes())!.len == 2
	assert base32.decode('AAAAA==='.bytes())!.len == 3
	assert base32.decode('AAAAAAA='.bytes())!.len == 4
}

// A quantum shorter than 8 characters without full padding is an error.
fn test_decode_rejects_missing_padding() {
	for s in ['=', 'A', 'AA', 'AAA', 'AAAA', 'AAAAA', 'AAAAAA', 'AAAAAAA'] {
		if _ := base32.decode(s.bytes()) {
			assert false, '${s} must be rejected'
		}
	}
	// Eight characters is the first well-formed quantum.
	assert base32.decode('AAAAAAAA'.bytes())!.len == 5
}

struct ErrorCase {
	input    string
	expected string
}

fn test_decode_error_messages_name_the_offset() {
	cases := [
		ErrorCase{'MZXW6=', 'illegal base32 data at input byte 6'},
		ErrorCase{'MZXW6==', 'illegal base32 data at input byte 7'},
		ErrorCase{'AAA=====', 'illegal base32 data at input byte 3'},
		ErrorCase{'AAAAAA==', 'illegal base32 data at input byte 6'},
	]
	for c in cases {
		base32.decode(c.input.bytes()) or {
			assert err.msg() == c.expected, c.input
			continue
		}
		assert false, '${c.input} must be rejected'
	}
}

// Extra padding after a complete quantum is currently accepted, but a padding
// byte read as a data symbol is rejected by the alphabet validation.
fn test_decode_padding_bytes_are_not_data_symbols() {
	assert base32.decode('MZXW6===='.bytes())!.bytestr() == 'foo'
	assert base32.decode('MZXW6====='.bytes())!.bytestr() == 'foo'
	if _ := base32.decode('MY=========='.bytes()) {
		assert false, 'padding read as a data symbol must be rejected'
	}
}

// The doc comment on `decode` says "\r and \n are ignored", and now the
// implementation matches it. See base32_newlines_test.v for the full set.
fn test_decode_ignores_newlines() {
	assert base32.decode('MZXW\n6==='.bytes())! == 'foo'.bytes()
	assert base32.decode('\nMZXW6===\n'.bytes())! == 'foo'.bytes()
}

fn test_decode_rejects_bytes_outside_the_alphabet() {
	for source in ['MZX?6===', 'MZXW?6==='] {
		if _ := base32.decode(source.bytes()) {
			assert false, source
		}
	}
	hex_encoding := base32.new_encoding(base32.hex_alphabet)
	if _ := hex_encoding.decode('CPNM?==='.bytes()) {
		assert false, 'unknown byte in hex alphabet must be rejected'
	}
	assert hex_encoding.decode('CPNMU==='.bytes())! == 'foo'.bytes()
}

// Standard base32 is case sensitive; lowercase input is outside its alphabet.
fn test_decode_does_not_case_fold() {
	if _ := base32.decode('mzxw6==='.bytes()) {
		assert false, 'lowercase input must be rejected'
	}
}
