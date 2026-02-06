// Copyright (c) 2022, 2024 blackshirt. All rights reserved.
// Use of this source code is governed by a MIT License
// that can be found in the LICENSE file.
module asn1

import encoding.utf8

// default_utf8string_tag is the default tag of ASN.1 UTF8STRING type.
pub const default_utf8string_tag = Tag{.universal, false, int(TagType.utf8string)}

// ASN.1 UNIVERSAL CLASS OF UTF8STRING TYPE.
//
// UTF8STRING is UTF8 unicode charset
pub struct Utf8String {
pub:
	value string
}

// new creates a new Utf8String element from string s.
pub fn Utf8String.new(s string) !Utf8String {
	if !utf8.validate_str(s) {
		return error('Utf8String: invalid UTF-8 string')
	}
	return Utf8String{
		value: s
	}
}

// from_bytes creates a new Utf8String element from bytes in src.
pub fn Utf8String.from_bytes(src []u8) !Utf8String {
	if !utf8.validate_str(src.bytestr()) {
		return error('Utf8String: invalid UTF-8 string')
	}
	return Utf8String{
		value: src.bytestr()
	}
}

// tag returns the tag of Utf8String element.
pub fn (uts Utf8String) tag() Tag {
	return default_utf8string_tag
}

// payload returns the payload of Utf8String element.
pub fn (uts Utf8String) payload() ![]u8 {
	return uts.payload_with_rule(.der)!
}

fn (uts Utf8String) str() string {
	if uts.value.len == 0 {
		return 'Utf8String (<empty>)'
	}
	return 'Utf8String (${uts.value})'
}

fn (uts Utf8String) payload_with_rule(rule EncodingRule) ![]u8 {
	if rule != .der && rule != .ber {
		return error('Utf8String: Unsupported rule')
	}
	if !utf8.validate_str(uts.value) {
		return error('Utf8String: invalid UTF-8 string')
	}
	return uts.value.bytes()
}

fn Utf8String.parse(mut p Parser) !Utf8String {
	tag := p.read_tag()!
	if !tag.equal(default_utf8string_tag) {
		return error('Bad Utf8String tag')
	}
	length := p.read_length()!
	bytes := p.read_bytes(length)!

	res := Utf8String.from_bytes(bytes)!

	return res
}

fn Utf8String.decode(src []u8) !(Utf8String, int) {
	return Utf8String.decode_with_rule(src, .der)!
}

fn Utf8String.decode_with_rule(bytes []u8, rule EncodingRule) !(Utf8String, int) {
	tag, length_pos := Tag.decode_with_rule(bytes, 0, rule)!
	if !tag.equal(default_utf8string_tag) {
		return error('Unexpected non-utf8string tag')
	}
	length, content_pos := Length.decode_with_rule(bytes, length_pos, rule)!
	content := if length == 0 {
		[]u8{}
	} else {
		// non-null length should contains non-null bytes
		if content_pos >= bytes.len || content_pos + length > bytes.len {
			return error('Utf8String: truncated payload bytes')
		}
		unsafe { bytes[content_pos..content_pos + length] }
	}

	ust := Utf8String.from_bytes(content)!
	next := content_pos + length

	return ust, next
}
