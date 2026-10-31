// Copyright (c) 2022, 2024 blackshirt. All rights reserved.
// Use of this source code is governed by a MIT License
// that can be found in the LICENSE file.
module asn1

// examples data was taken examples from https://letsencrypt.org/id/docs/a-warm-welcome-to-asn1-and-der/#explicit-vs-implicit
fn test_decode_element_with_or_without_options() ! {
	// Utf8String with tag == 12 (0c)
	original_obj := Utf8String.new('hi')!

	// without options
	normal := [u8(0x0C), 0x02, 0x68, 0x69]
	obj_1 := decode(normal)!
	assert obj_1.equal(original_obj)

	// with implicit definded as [5] IMPLICIT UTF8String was serialized into 85 02 68 69
	implicit_data := [u8(0x85), 0x02, 0x68, 0x69]
	obj_2 := decode_with_options(implicit_data, 'context_specific:5;implicit;inner:12')!

	assert obj_2.equal(original_obj)
	// dump(obj_2) // Output: obj_2: asn1.Element(Utf8String: (hi))

	// as explicit tagging defined as [5] EXPLICIT UTF8String encoded into A5 04 0C 02 68 69
	explicit_data := [u8(0xA5), 0x04, 0x0C, 0x02, 0x68, 0x69]
	obj_3 := decode_with_options(explicit_data, 'context_specific:5;explicit;inner:0x0c')!

	assert obj_3.equal(original_obj)
	// dump(obj_3) // output: obj_3: asn1.Element(Utf8String: (hi))
}

// test_decode_rejects_trailing_data checks that decode() reports trailing
// bytes after the element instead of silently accepting them, as documented.
// See https://github.com/vlang/v/issues/30184
fn test_decode_rejects_trailing_data() {
	// Utf8String "hi" (0x0C 0x02 'h' 'i') plus two trailing garbage bytes
	trailing := [u8(0x0C), 0x02, 0x68, 0x69, 0xDE, 0xAD]
	if _ := decode(trailing) {
		assert false, 'decode accepted trailing data'
	} else {
		assert err.msg() == 'decode on data with trailing data'
	}
	// the empty-options path of decode_with_options enforces the same rule
	if _ := decode_with_options(trailing, '') {
		assert false, 'decode_with_options accepted trailing data'
	} else {
		assert err.msg() == 'decode on data with trailing data'
	}
	// an exact element still decodes
	original_obj := Utf8String.new('hi')!
	obj := decode(trailing[..trailing.len - 2])!
	assert obj.equal(original_obj)
	for suffix in [[u8(0)], [u8(0x05), 0x00]] {
		mut bytes := trailing[..4].clone()
		bytes << suffix
		if _ := decode(bytes) {
			assert false, 'accepted trailing ${suffix}'
		}
		if _ := decode_with_options(bytes, '') {
			assert false, 'accepted trailing ${suffix} with empty options'
		}
	}
	// The nonempty options path already checks that its wrapper consumes all input.
	for bytes in [[u8(0xa5), 0x04, 0x0c, 0x02, 0x68, 0x69, 0], [u8(0x85), 0x02, 0x68, 0x69, 0]] {
		options := if bytes[0] == 0xa5 {
			'context_specific:5;explicit;inner:12'
		} else {
			'context_specific:5;implicit;inner:12'
		}
		if _ := decode_with_options(bytes, options) {
			assert false, 'accepted trailing bytes with ${options}'
		}
	}
}
