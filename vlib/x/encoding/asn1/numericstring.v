// Copyright (c) 2022, 2024 blackshirt. All rights reserved.
// Use of this source code is governed by a MIT License
// that can be found in the LICENSE file.
module asn1

// default_numericstring_tag is the default tag of ASN.1 NUMERICSTRING type.
pub const default_numericstring_tag = Tag{.universal, false, int(TagType.numericstring)}

// NumericString.
//
// NumericString was restricted character string types
// restricted to sequences of zero, one or more characters from some
// specified collection of characters.
// That was : digit : 0,1,..9 and spaces char (0x20)
pub struct NumericString {
pub:
	value string
}

// new creates a new NumericString element from string s.
pub fn NumericString.new(s string) !NumericString {
	if !all_numeric_string(s.bytes()) {
		return error('NumericString: contains non-numeric string')
	}
	return NumericString{
		value: s
	}
}

// tag returns the tag of NumericString element.
pub fn (nst NumericString) tag() Tag {
	return default_numericstring_tag
}

// payload returns the payload of NumericString element.
pub fn (nst NumericString) payload() ![]u8 {
	return nst.payload_with_rule(.der)!
}

fn (nst NumericString) str() string {
	if nst.value.len == 0 {
		return 'NumericString (<empty>)'
	}
	return 'NumericString (${nst.value})'
}

fn (nst NumericString) payload_with_rule(rule EncodingRule) ![]u8 {
	bytes := nst.value.bytes()
	if !all_numeric_string(bytes) {
		return error('NumericString: contains non-numeric string')
	}
	if rule != .der && rule != .ber {
		return error('NumericString: bad rule')
	}
	return bytes
}

fn NumericString.parse(mut p Parser) !NumericString {
	tag := p.read_tag()!
	if !tag.equal(default_numericstring_tag) {
		return error('Bad NumericString tag')
	}
	length := p.read_length()!
	bytes := p.read_bytes(length)!

	res := NumericString.from_bytes(bytes)!

	return res
}

fn NumericString.from_bytes(bytes []u8) !NumericString {
	if !all_numeric_string(bytes) {
		return error('NumericString: contains non-numeric string')
	}
	return NumericString{
		value: bytes.bytestr()
	}
}

fn NumericString.decode(bytes []u8) !(NumericString, int) {
	ns, next := NumericString.decode_with_rule(bytes, .der)!
	return ns, next
}

fn NumericString.decode_with_rule(bytes []u8, rule EncodingRule) !(NumericString, int) {
	tag, length_pos := Tag.decode_with_rule(bytes, 0, rule)!
	if !tag.equal(default_numericstring_tag) {
		return error('Unexpected non-numericstring tag')
	}
	length, content_pos := Length.decode_with_rule(bytes, length_pos, rule)!
	content := if length == 0 {
		[]u8{}
	} else {
		if content_pos >= bytes.len || content_pos + length > bytes.len {
			return error('NumericString: truncated payload bytes')
		}
		unsafe { bytes[content_pos..content_pos + length] }
	}

	ns := NumericString.from_bytes(content)!
	next := content_pos + length

	return ns, next
}

// Utility function
//
fn all_numeric_string(bytes []u8) bool {
	return bytes.all(is_numericstring(it))
}

fn is_numericstring(c u8) bool {
	return c.is_digit() || c == u8(0x20)
}
