// Copyright (c) 2022, 2024 blackshirt. All rights reserved.
// Use of this source code is governed by a MIT License
// that can be found in the LICENSE file.
module asn1

// default_visiblestring_tag is the default tag of ASN.1 VISIBLESTRING type.
pub const default_visiblestring_tag = Tag{.universal, false, int(TagType.visiblestring)}

// ASN.1 UNIVERSAL CLASS OF VISIBLESTRING TYPE.
//
// The ASN.1 VisibleString type supports a subset of ASCII characters that does not include control characters.
pub struct VisibleString {
pub:
	value string
}

// from_string creates a new VisibleString from string s.
pub fn VisibleString.new(s string) !VisibleString {
	if contains_ctrl_chars(s.bytes()) {
		return error('VisibleString: contains control chars')
	}
	return VisibleString{
		value: s
	}
}

// from_bytes creates a new VisibleString from bytes src
fn VisibleString.from_bytes(src []u8) !VisibleString {
	if contains_ctrl_chars(src) {
		return error('VisibleString: contains control chars')
	}
	return VisibleString{
		value: src.bytestr()
	}
}

// tag returns the tag of VisibleString type element.
pub fn (vst VisibleString) tag() Tag {
	return default_visiblestring_tag
}

// payload returns the payload of VisibleString type element.
pub fn (vst VisibleString) payload() ![]u8 {
	if contains_ctrl_chars(vst.value.bytes()) {
		return error('VisibleString: contains control chars')
	}
	return vst.value.bytes()
}

fn (vst VisibleString) str() string {
	if vst.value.len == 0 {
		return 'VisibleString (<empty>)'
	}
	return 'VisibleString (${vst.value})'
}

fn VisibleString.parse(mut p Parser) !VisibleString {
	tag := p.read_tag()!
	if !tag.equal(default_visiblestring_tag) {
		return error('Bad VisibleString tag')
	}
	length := p.read_length()!
	bytes := p.read_bytes(length)!

	res := VisibleString.from_bytes(bytes)!

	return res
}

fn VisibleString.decode(src []u8) !(VisibleString, int) {
	return VisibleString.decode_with_rule(src, .der)!
}

fn VisibleString.decode_with_rule(bytes []u8, rule EncodingRule) !(VisibleString, int) {
	tag, length_pos := Tag.decode_with_rule(bytes, 0, rule)!
	if !tag.equal(default_visiblestring_tag) {
		return error('Unexpected non-visiblestring tag')
	}
	length, content_pos := Length.decode_with_rule(bytes, length_pos, rule)!
	content := if length == 0 {
		[]u8{}
	} else {
		if content_pos >= bytes.len || content_pos + length > bytes.len {
			return error('VisibleString: truncated payload bytes')
		}
		unsafe { bytes[content_pos..content_pos + length] }
	}

	vst := VisibleString.from_bytes(content)!
	next := content_pos + length

	return vst, next
}

// Utility function for VisibleString
//

fn is_ctrl_char(c u8) bool {
	return (c >= 0 && c <= 0x1f) || c == 0x7f
}

fn contains_ctrl_chars(src []u8) bool {
	return src.any(is_ctrl_char(it))
}
